-- TODO: re-export other modules?

module PureAgentsAct (
    module Data.Maybe,
    module Control.Monad.STM,
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
    addNeighbours,
    startSimulation,
    createAgent,
    extractHdlEnv,
    observeAgentStates
  ) where

import System.Random

import Data.Maybe
import qualified Data.Map as Map

import Control.Monad.STM

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
-- The super-set of all Agent-Messages
data Msg m = Dt Double | Terminate | Domain m
-- An agent-message is always a tuple of a message with the sender-id
type AgentMessage m = (AgentId, Msg m)

-- NOTE: the central agent-behaviour-function: transforms an agent using a message and an environment to a new agent
type AgentTransformer m s e = ((Agent m s e, e) -> AgentMessage m -> STM (Agent m s e))
type OutFunc m s e = ((Map.Map AgentId (Agent m s e), e) -> IO (Bool, Double))

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
type AgentId = Int

data Agent m s e = Agent {
    agentId :: AgentId,
    killFlag :: Bool,
    msgBox :: TQueue (AgentMessage m),
    neighbours :: Map.Map AgentId (TQueue (AgentMessage m)),
    trans :: AgentTransformer m s e,
    state :: s,
    newAgents :: [Agent m s e]
}

data SimHandle m s e = SimHandle {
    simHdlAgents :: Map.Map AgentId (Async (Agent m s e), TVar s, TQueue (AgentMessage m)),
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
    | otherwise = putMessage receiverQueue senderId (Domain m)
    where
        senderId = agentId a
        mayReceiverQueue = Map.lookup receiverId (neighbours a)
        receiverNotFound = isNothing mayReceiverQueue
        receiverQueue = fromJust mayReceiverQueue

broadcastMsgToNeighbours :: Agent m s e -> m -> STM ()
broadcastMsgToNeighbours a m = do
                                mapM (\neighbourBox -> putMessage neighbourBox senderId (Domain m)) (Map.elems (neighbours a))
                                return ()
    where
        senderId = agentId a

sendMsgToRandomNeighbour :: (RandomGen g) => Agent m s e -> m -> g -> STM g
sendMsgToRandomNeighbour a m g = do
                                    putMessage neighbourBox senderId (Domain m)
                                    return g'
    where
        senderId = agentId a
        ns = neighbours a
        nsCount = Map.size ns
        (randIdx, g') = randomR(0, nsCount - 1) g
        neighbourBox = (Map.elems ns) !! randIdx

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

observeAgentStates :: SimHandle m s e -> IO [s]
observeAgentStates hdl =  undefined -- TODO: implement

awaitAgentsTermination :: SimHandle m s e -> IO [Agent m s e]
awaitAgentsTermination hdl = undefined -- TODO: implement

terminateAgents :: SimHandle m s e -> IO [Agent m s e]
terminateAgents hdl = undefined -- TODO: implement

startSimulation :: [Agent m s e] -> Double -> e -> IO (SimHandle m s e)
startSimulation as dt e = do
                            sas <- mapM (startAgent dt e) as
                            let am = foldl (\accM (aid, asyncHdl, sVar, chan) -> Map.insert aid (asyncHdl, sVar, chan) accM) Map.empty sas
                            let hdl = SimHandle { simHdlAgents = am, simHdlEnv = e }
                            return hdl

    where
        -- allows to observe the state of an agent
        startAgent :: Double -> e -> Agent m s e -> IO (AgentId, Async (Agent m s e), TVar s, TQueue (AgentMessage m))
        startAgent dt e a = do
                                let s = state a
                                let aid = agentId a
                                let chan = msgBox a
                                sVar <- newTVarIO s
                                asyncHdl <- async (asyncRunAgent dt e sVar a)
                                return (aid, asyncHdl, sVar, chan)

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
asyncRunAgent :: Double -> e -> TVar s -> Agent m s e -> IO (Agent m s e)
asyncRunAgent dt e sVar a = asyncRunAgent' 0 dt e sVar a
    where
        asyncRunAgent' :: Double -> Double -> e ->  TVar s -> Agent m s e -> IO (Agent m s e)
        asyncRunAgent' t dt e sVar a = do
                                        a' <- transactAgent t e a
                                        let s = state a'
                                        atomically $ writeTVar sVar s
                                        let nas = newAgents a'
                                        let kill = killFlag a'
                                        -- TODO: handle new agents
                                        -- TODO: handle killing of this agent
                                        asyncRunAgent' (t+dt) dt e sVar a'

-- NOTE: every agent must be run inside an atomic-block to 'commit' its actions. We don't want to run the agent in the IO but pull this out
-- NOTE: here we see that due to atomically ALL will have to run in IO! => use of ParIO
transactAgent :: Double -> e -> Agent m s e -> IO (Agent m s e)
transactAgent t e a = atomically $ (stepAgent t e a)

stepAgent :: Double -> e -> Agent m s e -> STM (Agent m s e)
stepAgent t e a = do
                    aAfterMsgProc <- processAllMessages (a, e)
                    let agentTransformer = trans aAfterMsgProc
                    aAfterUpdt <- agentTransformer (aAfterMsgProc, e) (-1, Dt t)
                    return aAfterUpdt

processAllMessages :: (Agent m s e, e) -> STM (Agent m s e)
processAllMessages ae@(a, e) = do
                                mayMsg <- tryReadTQueue (msgBox a)
                                if (isNothing mayMsg) then
                                    return a
                                    else
                                        do
                                            let msg = fromJust mayMsg
                                            a' <- processMsg ae msg
                                            processAllMessages (a', e)

processMsg :: (Agent m s e, e) -> (AgentMessage m) -> STM (Agent m s e)
processMsg ae@(a, e) msg = agentTransformer ae msg
    where
        agentTransformer = trans a
------------------------------------------------------------------------------------------------------------------------
