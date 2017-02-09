module PureAgentsAct (
    Agent(..),
    SimHandle(..),
    AgentId,
    AgentTransformer,
    Event(..),
    kill,
    newAgent,
    sendMsg,
    sendMsgToRandomNeighbour,
    broadcastMsgToNeighbours,
    updateState,
    writeState,
    addNeighbours,
    startSimulation,
    createAgent,
    extractHdlEnv,
    observeAgentStates
  ) where

import System.Random

import Data.Maybe
import qualified Data.Map as Map

import GHC.Conc
import Control.Monad.STM
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
data Event m = Start | Terminate | Dt (Double, Double) | Message (AgentId, m)

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
    msgBox :: TQueue (Event m),
    neighbours :: Map.Map AgentId (TQueue (Event m)),
    trans :: AgentTransformer m s e,
    state :: s,
    newAgents :: [Agent m s e]
}

type GlobalAgentInfo m s e = (Async (Agent m s e), TVar (Double, s), TQueue (Event m))
type GlobalAgentCollection m s e = TVar (Map.Map AgentId (GlobalAgentInfo m s e))

data SimHandle m s e = SimHandle {
    simHdlAgents :: GlobalAgentCollection m s e,
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
        neighbourBox = (Map.elems ns) !! randIdx

putMessage :: TQueue (Event m) -> AgentId -> m -> STM ()
putMessage q senderId m = writeTQueue q (Message (senderId, m))

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

observeAgentStates :: SimHandle m s e -> IO [(AgentId, Double, s)]
observeAgentStates hdl =  do
                            let amVar = simHdlAgents hdl
                            am <- readTVarIO amVar
                            ss <- mapM (\(aid, (_, sVar, _)) -> do
                                                            (t, s) <- readTVarIO sVar
                                                            return (aid, t, s)) (Map.assocs am)
                            return ss

awaitAgentsTermination :: SimHandle m s e -> IO [Agent m s e]
awaitAgentsTermination hdl = undefined -- TODO: implement by calling await on all agents

terminateAgents :: SimHandle m s e -> IO [Agent m s e]
terminateAgents hdl = undefined -- TODO: implement by sending Terminate to all agents

startSimulation :: [Agent m s e] -> Double -> e -> IO (SimHandle m s e)
startSimulation as dt e = do
                            gacVar <- newTVarIO Map.empty
                            startAndAddAgents dt e gacVar as
                            let hdl = SimHandle { simHdlAgents = gacVar, simHdlEnv = e }
                            return hdl

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
startAndAddAgents :: Double -> e -> GlobalAgentCollection m s e -> [Agent m s e] -> IO ()
startAndAddAgents dt e gacVar as = do
                                    sas <- mapM (startAgent dt e gacVar) as
                                    gacMap <- readTVarIO gacVar
                                    let gacMap' = foldl (\accM (aid, (asyncHdl, sVar, chan)) -> Map.insert aid (asyncHdl, sVar, chan) accM) gacMap sas
                                    atomically $ writeTVar gacVar gacMap'

startAgent :: Double -> e -> GlobalAgentCollection m s e -> Agent m s e -> IO (AgentId, GlobalAgentInfo m s e)
startAgent dt e gac a = do
                            let s = state a
                            let aid = agentId a
                            let chan = msgBox a
                            sVar <- newTVarIO (0.0, s)
                            atomically $ writeTQueue chan Start
                            asyncHdl <- async (asyncRunAgent dt e sVar gac a)
                            return (aid, (asyncHdl, sVar, chan))

asyncRunAgent :: Double -> e -> TVar (Double, s) -> GlobalAgentCollection m s e -> Agent m s e -> IO (Agent m s e)
asyncRunAgent dt e sVar gac a = asyncRunAgent' (0, dt) e sVar gac a
    where
        asyncRunAgent' :: (Double, Double) -> e ->  TVar (Double, s) -> GlobalAgentCollection m s e -> Agent m s e -> IO (Agent m s e)
        asyncRunAgent' tdt@(t, dt) e sVar gac a = do
                                                    (a', continue) <- transactAgent tdt e a

                                                    atomically $ writeTVar sVar (t, (state a'))

                                                    a'' <- handleNewAgents a' dt e gac

                                                    kill <- handleAgentKill a'' gac

                                                    threadDelay 1000   -- NOTE: need SOME delay, otherwise system would grind to a halt (1000 seems to be enough)

                                                    if (continue && not kill) then
                                                        asyncRunAgent' (t+dt, dt) e sVar gac a''
                                                        else
                                                            return a''

        handleNewAgents :: Agent m s e -> Double -> e -> GlobalAgentCollection m s e -> IO (Agent m s e)
        handleNewAgents a dt e gac = do
                                        startAndAddAgents dt e gac nas
                                        return $ a { newAgents = [] }
            where
                nas = newAgents a

        handleAgentKill :: Agent m s e -> GlobalAgentCollection m s e -> IO (Bool)
        handleAgentKill a gacVar
            | kill = do
                        gac <- readTVarIO gacVar
                        let gac' = Map.delete aid gac
                        atomically $ writeTVar gacVar gac'
                        return True

            | otherwise = return False
            where
                kill = killFlag a
                aid = agentId a


-- NOTE: every agent must be run inside an atomic-block to 'commit' its actions. We don't want to run the agent in the IO but pull this out
-- NOTE: here we see that due to atomically ALL will have to run in IO! => use of ParIO
transactAgent :: (Double, Double) -> e -> Agent m s e -> IO (Agent m s e, Bool)
transactAgent tdt e a = atomically $ (stepAgent tdt e a)

stepAgent :: (Double, Double) -> e -> Agent m s e -> STM (Agent m s e, Bool)
stepAgent tdt e a = do
                        (aAfterMsgProc, cont) <- processAllMessages (a, e)
                        let agentTransformer = trans aAfterMsgProc
                        aAfterUpdt <- agentTransformer (aAfterMsgProc, e) (Dt tdt)
                        return (aAfterUpdt, cont)

processAllMessages :: (Agent m s e, e) -> STM (Agent m s e, Bool)
processAllMessages ae@(a, e) = do
                                mayMsg <- tryReadTQueue (msgBox a)
                                maybe (return (a, True)) (processJustMessage ae) mayMsg
    where
        processJustMessage :: (Agent m s e, e) -> (Event m) -> STM (Agent m s e, Bool)
        processJustMessage ae@(a, e) msg = do
                                                (a', cont) <- processMsg ae msg
                                                if (cont) then
                                                    processAllMessages (a', e)
                                                    else
                                                        return (a', False)

processMsg :: (Agent m s e, e) -> (Event m) -> STM (Agent m s e, Bool)
processMsg ae@(a, e) Terminate = return (a, False)
processMsg ae@(a, e) msg = do
                            a' <- agentTransformer ae msg
                            return (a', True)
    where
        agentTransformer = trans a
------------------------------------------------------------------------------------------------------------------------
