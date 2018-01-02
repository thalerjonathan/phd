module PureAgentsConc (
    Agent(..),
    SimHandle(..),
    AgentId,
    AgentTransformer,
    AgentMessage,
    Event(..),
    kill,
    newAgent,
    sendMsg,
    sendMsgToRandomNeighbour,
    broadcastMsgToNeighbours,
    updateState,
    writeState,
    addNeighbours,
    stepSimulation,
    initStepSimulation,
    advanceSimulation,
    runSimulation,
    createAgent,
    extractHdlEnv,
    extractHdlAgents
  ) where

import System.Random

import Data.Maybe
import qualified Data.Map as Map

import Control.Monad.STM

import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
data Event m = Start | Dt Double | Message (AgentId, m)
-- An agent-message is always a tuple of a message with the sender-id
type AgentMessage m = (AgentId, m)

-- NOTE: the central agent-behaviour-function: transforms an agent using a message and an environment to a new agent
type AgentTransformer m s e = ((Agent m s e, e) -> Event m -> STM (Agent m s e))
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
    simHdlEnv :: e
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

broadcastMsgToNeighbours :: Agent m s e -> m -> STM ()
broadcastMsgToNeighbours a m = do
                                mapM (\neighbourBox -> putMessage neighbourBox senderId m) (Map.elems (neighbours a))
                                return ()
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

extractHdlEnv :: SimHandle m s e -> e
extractHdlEnv = simHdlEnv

extractHdlAgents :: SimHandle m s e -> [Agent m s e]
extractHdlAgents = Map.elems . simHdlAgents

-- TODO: return all steps of agents and environment
stepSimulation :: [Agent m s e] -> e -> Double -> Int -> IO ([Agent m s e])
stepSimulation as e dt n = do
                            let am = insertAgents Map.empty as
                            am' <- sendEvent am e Start
                            am'' <- stepSimulation' am' e dt n
                            return (Map.elems am'')
    where
        stepSimulation' :: Map.Map AgentId (Agent m s e) -> e -> Double -> Int -> IO (Map.Map AgentId (Agent m s e))
        stepSimulation' am e dt 0 = return am
        stepSimulation' am e dt n = do
                                        am' <- stepAllAgents am dt e
                                        stepSimulation' am' e dt (n-1)

initStepSimulation :: [Agent m s e] -> e -> IO (SimHandle m s e)
initStepSimulation as e = do
                            am' <- sendEvent am e Start
                            return SimHandle { simHdlAgents = am', simHdlEnv = e }
    where
        am = insertAgents Map.empty as

advanceSimulation :: SimHandle m s e -> Double -> IO (SimHandle m s e)
advanceSimulation hdl dt = do
                            let e = extractHdlEnv hdl
                            let am = simHdlAgents hdl
                            am' <- stepAllAgents am dt e
                            return hdl { simHdlAgents = am' } -- NOTE: no need to update e' because implicitly done

runSimulation :: [Agent m s e] -> e -> OutFunc m s e -> IO ()
runSimulation as e out = do
                            let am = insertAgents Map.empty as
                            am' <- sendEvent am e Start
                            runSimulation' am' 0.0 e out

    where
        runSimulation' :: Map.Map AgentId (Agent m s e) -> Double -> e -> OutFunc m s e -> IO ()
        runSimulation' as dt e out = do
                                        am' <- stepAllAgents as dt e
                                        (cont, dt') <- out (am', e)
                                        if cont == True then
                                            runSimulation' am' dt' e out
                                            else
                                                return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
sendEvent :: Map.Map AgentId (Agent m s e) -> e -> Event m -> IO (Map.Map AgentId (Agent m s e))
sendEvent am e event = do
                        let as = Map.elems am
                        asyncAs <- mapM (async . (sendEventMap event e)) as
                        syncedAs <- mapM wait asyncAs
                        let am' = insertAgents Map.empty syncedAs
                        return am'

    where
        sendEventMap :: Event m -> e -> (Agent m s e) -> IO (Agent m s e)
        sendEventMap event e a = atomically $ agentTransformer (a, e) event
            where
                agentTransformer = (trans a)

insertAgents :: Map.Map AgentId (Agent m s e) -> [Agent m s e] -> Map.Map AgentId (Agent m s e)
insertAgents am as = foldl (\accMap a -> Map.insert (agentId a) a accMap ) am as

-- NOTE: iteration order makes no difference as they are in fact 'parallel': no agent can see the update of others, all happens at the same time
stepAllAgents :: Map.Map AgentId (Agent m s e) -> Double -> e -> IO (Map.Map AgentId (Agent m s e))
stepAllAgents am dt e = do
                            am' <- runAgentsParallel dt e am
                            let (newAgents, amClearedNewAgents) = collectAndClearNewAgents am'
                            let amWithNewAgents = insertAgents amClearedNewAgents newAgents
                            let amRemovedKilled = Map.foldl killAgentFold Map.empty amWithNewAgents
                            return amRemovedKilled

runAgentsParallel :: Double -> e -> Map.Map AgentId (Agent m s e) -> IO (Map.Map AgentId (Agent m s e))
runAgentsParallel dt e am = do
                                let as = Map.elems am
                                asyncAs <- mapM (async . runFunc) as
                                syncedAs <- mapM wait asyncAs
                                let am' = insertAgents Map.empty syncedAs
                                return am'
                                    where
                                        runFunc = transactAgent dt e

-- NOTE: every agent must be run inside an atomic-block to 'commit' its actions. We don't want to run the agent in the IO but pull this out
-- NOTE: here we see that due to atomically ALL will have to run in IO! => use of ParIO
transactAgent :: Double -> e -> Agent m s e -> IO (Agent m s e)
transactAgent dt e a = atomically $ (stepAgent dt e a)

stepAgent :: Double -> e -> Agent m s e -> STM (Agent m s e)
stepAgent dt e a = do
                    aAfterMsgProc <- processAllMessages (a, e)
                    let agentTransformer = trans aAfterMsgProc
                    aAfterUpdt <- agentTransformer (aAfterMsgProc, e) (Dt dt)
                    return aAfterUpdt

processAllMessages :: (Agent m s e, e) -> STM (Agent m s e)
processAllMessages (a, e) = do
                                mayMsg <- tryReadTQueue (msgBox a)
                                if (isNothing mayMsg) then
                                    return a
                                    else
                                        do
                                            let msg = fromJust mayMsg
                                            a' <- processMsg (a, e) msg
                                            processAllMessages (a', e)

processMsg :: (Agent m s e, e) -> (AgentId, m) -> STM (Agent m s e)
processMsg (a, e) (senderId, m) = agentTransformer (a, e) (Message (senderId, m))
    where
        agentTransformer = trans a
------------------------------------------------------------------------------------------------------------------------

killAgentFold :: Map.Map AgentId (Agent m s e) -> Agent m s e -> Map.Map AgentId (Agent m s e)
killAgentFold am a
    | killFlag a = am
    | otherwise = Map.insert (agentId a) a am

collectAndClearNewAgents :: Map.Map AgentId (Agent m s e) -> ([(Agent m s e)], Map.Map AgentId (Agent m s e))
collectAndClearNewAgents am = Map.foldl (\(newAsAcc, amAcc) a -> (newAsAcc ++ (newAgents a), Map.insert (agentId a) a { newAgents = [] } amAcc)) ([], am) am


