module PureAgentsConc (
    module Data.Maybe,
    Agent(..),
    SimHandle(..),
    AgentId,
    AgentTransformer,
    AgentMessage,
    Msg(..),
    kill,
    newAgent,
    sendMsg,
    sendMsgToRandomNeighbour,
    broadcastMsgToNeighbours,
    updateState,
    writeState,
    readEnv,
    writeEnv,
    changeEnv,
    addNeighbours,
    stepSimulation,
    initStepSimulation,
    advanceSimulation,
    runSimulation,
    createAgent,
    extractHdlEnv,
    extractHdlAgents
  ) where

import Data.Maybe
import System.Random

import Control.Concurrent.Async

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

import qualified Data.Map as Map

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
-- The super-set of all Agent-Messages
data Msg m = Dt Double | Domain m
-- An agent-message is always a tuple of a message with the sender-id
type AgentMessage m = (AgentId, Msg m)

-- NOTE: the central agent-behaviour-function: transforms an agent using a message and an environment to a new agent
type AgentTransformer m s e = ((Agent m s e, TVar e) -> AgentMessage m -> STM (Agent m s e))
type OutFunc m s e = ((Map.Map AgentId (Agent m s e), e) -> IO (Bool, Double))

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
type AgentId = Int

data Agent m s e = Agent {
    agentId :: AgentId,
    killFlag :: Bool,
    msgBox :: TQueue (AgentId, m),
    neighbours :: Map.Map AgentId (TQueue (AgentId, m)),
    trans :: AgentTransformer m s e,
    state :: s,
    newAgents :: [Agent m s e]
}

data SimHandle m s e = SimHandle {
    simHdlAgents :: Map.Map AgentId (Agent m s e),
    simHdlEnv :: TVar e
}

newAgent :: Agent m s e -> Agent m s e -> Agent m s e
newAgent aParent aNew = aParent { newAgents = nas ++ [aNew] }
    where
        nas = newAgents aParent

kill :: Agent m s e -> Agent m s e
kill a = a { killFlag = True }

sendMsg :: Agent m s e -> m -> AgentId -> STM ()
sendMsg a m receiverId
    | receiverNotFound = return ()
    | otherwise = putMessage receiverQueue senderId m
    where
        senderId = agentId a
        mayReceiverQueue = Map.lookup receiverId (neighbours a)
        receiverNotFound = isNothing mayReceiverQueue
        receiverQueue = fromJust mayReceiverQueue

-- TODO: implement
broadcastMsgToNeighbours :: Agent m s e -> m -> STM ()
broadcastMsgToNeighbours a m = return () {-do
                                m <- Map.map (\neighbourBox -> putMessage neighbourBox senderId m) (neighbours a)
                                return ()-}
    where
        senderId = agentId a

sendMsgToRandomNeighbour :: (RandomGen g) => Agent m s e -> m -> g -> STM g
sendMsgToRandomNeighbour a m g = do
                                    putMessage neighbourBox senderId m
                                    return g'
    where
        senderId = agentId a
        ns = neighbours a
        nsCount = Map.size ns
        (randIdx, g') = randomR(0, nsCount - 1) g
        neighbourBox = (Map.elems ns) !! randIdx     -- TODO: can we optimize this?

putMessage :: TQueue (AgentId, m) -> AgentId -> m -> STM ()
putMessage q senderId m = writeTQueue q (senderId, m)

updateState :: Agent m s e -> (s -> s) -> Agent m s e
updateState a sf = a { state = s' }
    where
        s = state a
        s' = sf s

writeState :: Agent m s e -> s -> Agent m s e
writeState a s = updateState a (\_ -> s)

createAgent :: AgentId -> s -> AgentTransformer m s e -> STM (Agent m s e)
createAgent i s t = do
                        tq <- newTQueue
                        return Agent{ agentId = i,
                                    state = s,
                                    msgBox = tq,
                                    killFlag = False,
                                    neighbours = Map.empty,
                                    newAgents = [],
                                    trans = t }

addNeighbours :: Agent m s e -> [Agent m s e] -> Agent m s e
addNeighbours a ns = a { neighbours = newNeighbours }
    where
        newNeighbours = foldl (\acc a -> Map.insert (agentId a) (msgBox a) acc) (neighbours a) ns

readEnv :: TVar e -> STM e
readEnv eVar = readTVar eVar

writeEnv :: TVar e -> e -> STM ()
writeEnv eVar e = changeEnv eVar (\_ -> e)

changeEnv :: TVar e -> (e -> e) -> STM ()
changeEnv eVar tx = modifyTVar eVar tx

extractHdlEnv :: SimHandle m s e -> TVar e
extractHdlEnv = simHdlEnv

extractHdlAgents :: SimHandle m s e -> [Agent m s e]
extractHdlAgents = Map.elems . simHdlAgents

-- TODO: return all steps of agents and environment
stepSimulation :: [Agent m s e] -> e -> Double -> Int -> IO ([Agent m s e], e)
stepSimulation as e dt n = do
                            eVar <- atomically $ newTVar e
                            let am = insertAgents Map.empty as
                            am' <- stepSimulation' am eVar dt n
                            e' <- atomically $ readTVar eVar
                            return (Map.elems am', e')
    where
        stepSimulation' :: Map.Map AgentId (Agent m s e) -> TVar e -> Double -> Int -> IO (Map.Map AgentId (Agent m s e))
        stepSimulation' am eVar dt 0 = return am
        stepSimulation' am eVar dt n = do
                                        am' <- stepAllAgents am dt eVar
                                        stepSimulation' am' eVar dt (n-1)

initStepSimulation :: [Agent m s e] -> e -> STM (SimHandle m s e)
initStepSimulation as e = do
                            eVar <- newTVar e
                            let am = insertAgents Map.empty as
                            return SimHandle { simHdlAgents = am, simHdlEnv = eVar }

advanceSimulation :: SimHandle m s e -> Double -> IO (SimHandle m s e)
advanceSimulation hdl dt = do
                            let eVar = extractHdlEnv hdl
                            let am = simHdlAgents hdl
                            am' <- stepAllAgents am dt eVar
                            return hdl { simHdlAgents = am' } -- NOTE: no need to update e' because implicitly done

runSimulation :: [Agent m s e] -> e -> OutFunc m s e -> IO ()
runSimulation as e out = do
                            eVar <- atomically $ newTVar e
                            let am = insertAgents Map.empty as
                            runSimulation' am 0.0 eVar out

    where
        runSimulation' :: Map.Map AgentId (Agent m s e) -> Double -> TVar e -> OutFunc m s e -> IO ()
        runSimulation' as dt eVar out = do
                                            am' <- stepAllAgents as dt eVar
                                            e <- atomically $ readTVar eVar
                                            (cont, dt') <- out (am', e)
                                            if cont == True then
                                                runSimulation' am' dt' eVar out
                                                else
                                                    return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
insertAgents :: Map.Map AgentId (Agent m s e) -> [Agent m s e] -> Map.Map AgentId (Agent m s e)
insertAgents am as = foldl (\accMap a -> Map.insert (agentId a) a accMap ) am as

-- NOTE: iteration order makes no difference as they are in fact 'parallel': no agent can see the update of others, all happens at the same time
stepAllAgents :: Map.Map AgentId (Agent m s e) -> Double -> TVar e -> IO (Map.Map AgentId (Agent m s e))
stepAllAgents am dt eVar = do
                            as <- runAgentsParallel dt eVar (Map.elems am)
                            let am' = insertAgents Map.empty as
                            let (newAgents, amClearedNewAgents) = collectAndClearNewAgents am'
                            let amWithNewAgents = insertAgents amClearedNewAgents newAgents
                            let amRemovedKilled = Map.foldl killAgentFold Map.empty amWithNewAgents
                            return amRemovedKilled

killAgentFold :: Map.Map AgentId (Agent m s e) -> Agent m s e -> Map.Map AgentId (Agent m s e)
killAgentFold am a
    | killFlag a = am
    | otherwise = Map.insert (agentId a) a am

collectAndClearNewAgents :: Map.Map AgentId (Agent m s e) -> ([(Agent m s e)], Map.Map AgentId (Agent m s e))
collectAndClearNewAgents am = Map.foldl (\(newAsAcc, amAcc) a -> (newAsAcc ++ (newAgents a), Map.insert (agentId a) a { newAgents = [] } amAcc)) ([], am) am

runAgentsParallel :: Double -> TVar e -> [Agent m s e] -> IO [(Agent m s e)]
runAgentsParallel dt eVar as = do
                                asyncAs <- mapM (async . runFunc) as
                                syncedAs <- mapM wait asyncAs
                                return syncedAs
                                    where
                                        runFunc = runAgent dt eVar
-- PROCESSING THE AGENT
-- NOTE: every agent must be run inside an atomic-block to 'commit' its actions. We don't want to run the agent in the IO but pull this out
-- NOTE: here we see that due to atomically ALL will have to run in IO! => use of ParIO
runAgent :: Double -> TVar e -> Agent m s e -> IO (Agent m s e)
runAgent dt eVar a = atomically $ (stepAgent dt eVar a)

stepAgent :: Double -> TVar e -> Agent m s e -> STM (Agent m s e)
stepAgent dt eVar a = do
                        aAfterMsgProc <- processAllMessages (a, eVar)
                        let agentTransformer = trans aAfterMsgProc
                        aAfterUpdt <- agentTransformer (aAfterMsgProc, eVar) (-1, Dt dt)
                        return aAfterUpdt

processAllMessages :: (Agent m s e, TVar e) -> STM (Agent m s e)
processAllMessages (a, eVar) = do
                                mayMsg <- tryReadTQueue (msgBox a)
                                if (isNothing mayMsg) then
                                    return a
                                    else
                                        do
                                            let msg = fromJust mayMsg
                                            a' <- processMsg (a, eVar) msg
                                            processAllMessages (a', eVar)

processMsg :: (Agent m s e, TVar e) -> (AgentId, m) -> STM (Agent m s e)
processMsg (a, eVar) (senderId, m) = agentTransformer (a, eVar) (senderId, Domain m)
    where
        agentTransformer = trans a
------------------------------------------------------------------------------------------------------------------------
