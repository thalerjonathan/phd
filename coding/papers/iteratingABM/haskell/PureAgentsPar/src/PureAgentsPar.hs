module PureAgentsPar (
    Agent(..),
    SimHandle(..),
    AgentId,
    GlobalEnvironment,
    LocalEnvironment,
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
    getLocalEnv,
    getLocalEnvById,
    updateLocalEnv,
    extractHdlEnv,
    extractHdlAgents
  ) where

import Data.Maybe
import System.Random

import Control.Parallel.Strategies

import qualified Data.Map as Map

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
data Event m = Start | Dt Double | Message (AgentId, m)
-- An agent-message is always a tuple of a message with the sender-id
type AgentMessage m = (AgentId, m)

type LocalEnvironment e = e
type GlobalEnvironment e = Map.Map AgentId (LocalEnvironment e)

-- NOTE: the central agent-behaviour-function: transforms an agent using a message and an environment to a new agent
type AgentTransformer m s e = ((Agent m s e, GlobalEnvironment e, LocalEnvironment e) -> Event m -> (Agent m s e, LocalEnvironment e))
type OutFunc m s e = ((Map.Map AgentId (Agent m s e), GlobalEnvironment e) -> IO (Bool, Double))

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
type AgentId = Int

data Agent m s e = Agent {
    agentId :: AgentId,
    killFlag :: Bool,
    outBox :: [(AgentId, AgentId, m)],              -- NOTE: 1st AgentId contains the senderId, 2nd AgentId contains the targetId (redundancy saves us processing time)
    inBox :: [(AgentId, m)],                        -- NOTE: AgentId contains the senderId
    neighbours :: Map.Map AgentId AgentId,
    trans :: AgentTransformer m s e,
    state :: s,
    newAgents :: [Agent m s e]
}

data SimHandle m s e = SimHandle {
    simHdlAgents :: Map.Map AgentId (Agent m s e),
    simHdlEnv :: GlobalEnvironment e
}

newAgent :: Agent m s e -> Agent m s e -> Agent m s e
newAgent aParent aNew = aParent { newAgents = aNew : nas }
    where
        nas = newAgents aParent

kill :: Agent m s e -> Agent m s e
kill a = a { killFlag = True }

-- NOTE: we are not checking if the receiver is actually in the neighbours
sendMsg :: Agent m s e -> m -> AgentId -> Agent m s e
sendMsg a m targetId = a { outBox = newOutBox }
    where
        senderId = agentId a
        newOutBox = (senderId, targetId, m) : (outBox a) -- TODO: is order irrelevant? could use ++ as well but more expensive (or not?)

broadcastMsgToNeighbours :: Agent m s e -> m -> Agent m s e
broadcastMsgToNeighbours a m = foldl (\a' nId -> sendMsg a' m nId ) a nsIds
    where
        nsIds = Map.elems (neighbours a)

sendMsgToRandomNeighbour :: (RandomGen g) => Agent m s e -> m -> g -> (Agent m s e, g)
sendMsgToRandomNeighbour a m g = (a', g')
    where
        ns = (neighbours a)
        nsCount = Map.size ns
        (randIdx, g') = randomR(0, nsCount-1) g
        randAgentId = (Map.elems ns) !! randIdx     -- TODO: do we really need to map to elements?
        a' = sendMsg a m randAgentId

updateState :: Agent m s e -> (s -> s) -> Agent m s e
updateState a sf = a { state = s' }
    where
        s = state a
        s' = sf s

writeState :: Agent m s e -> s -> Agent m s e
writeState a s = updateState a (\_ -> s)

getLocalEnv :: Agent m s e -> GlobalEnvironment e -> LocalEnvironment e
getLocalEnv a ge = getLocalEnvById aid ge
    where
        aid = agentId a

getLocalEnvById :: AgentId -> GlobalEnvironment e -> LocalEnvironment e
getLocalEnvById aid ge = fromJust leMay
    where
        leMay = Map.lookup aid ge

updateLocalEnv :: Agent m s e -> GlobalEnvironment e -> (LocalEnvironment e -> LocalEnvironment e) -> LocalEnvironment e
updateLocalEnv a ge tx = tx le
    where
        aid = agentId a
        leMay = Map.lookup aid ge
        le = fromJust leMay

createAgent :: AgentId -> s -> AgentTransformer m s e -> Agent m s e
createAgent i s t = Agent{ agentId = i,
                                    state = s,
                                    outBox = [],
                                    inBox = [],
                                    killFlag = False,
                                    neighbours = Map.empty,
                                    newAgents = [],
                                    trans = t }

addNeighbours :: Agent m s e -> [Agent m s e] -> Agent m s e
addNeighbours a ns = a { neighbours = newNeighbours }
    where
        newNeighbours = foldl (\acc a -> Map.insert (agentId a) (agentId a) acc) (neighbours a) ns

extractHdlEnv :: SimHandle m s e -> GlobalEnvironment e
extractHdlEnv = simHdlEnv

extractHdlAgents :: SimHandle m s e -> [Agent m s e]
extractHdlAgents = Map.elems . simHdlAgents

-- TODO: return all steps of agents and environment
stepSimulation :: [Agent m s e] -> GlobalEnvironment e -> Double -> Int -> ([Agent m s e], GlobalEnvironment e)
stepSimulation as ge dt n = (Map.elems am'', ge'')
    where
        am = insertAgents Map.empty as
        (am', ge') = sendEvent am ge Start
        (am'', ge'') = stepSimulation' am ge dt n

        stepSimulation' :: Map.Map AgentId (Agent m s e) -> GlobalEnvironment e -> Double -> Int -> (Map.Map AgentId (Agent m s e), GlobalEnvironment e)
        stepSimulation' am ge dt 0 = (am, ge)
        stepSimulation' am ge dt n = stepSimulation' am' ge' dt (n-1)
            where
                (am', ge') = stepAllAgents am dt ge

initStepSimulation :: [Agent m s e] -> GlobalEnvironment e -> SimHandle m s e
initStepSimulation as ge = hdl
    where
        am = insertAgents Map.empty as
        (am', ge') = sendEvent am ge Start
        hdl = SimHandle { simHdlAgents = am', simHdlEnv = ge' }

advanceSimulation :: SimHandle m s e -> Double -> SimHandle m s e
advanceSimulation hdl dt = hdl { simHdlAgents = am', simHdlEnv = ge' }
    where
        ge = extractHdlEnv hdl
        am = simHdlAgents hdl
        (am', ge') = stepAllAgents am dt ge

runSimulation :: [Agent m s e] -> GlobalEnvironment e -> OutFunc m s e -> IO ()
runSimulation as ge out = runSimulation' am' 0.0 ge' out
    where
        am = insertAgents Map.empty as
        (am', ge') = sendEvent am ge Start

        runSimulation' :: Map.Map AgentId (Agent m s e) -> Double -> GlobalEnvironment e -> OutFunc m s e -> IO ()
        runSimulation' as dt ge out = do
                                        let (am', ge') = stepAllAgents as dt ge
                                        (cont, dt') <- out (am', ge')
                                        if cont == True then
                                            runSimulation' am' dt' ge' out
                                            else
                                                return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
sendEvent :: Map.Map AgentId (Agent m s e) -> GlobalEnvironment e -> Event m -> (Map.Map AgentId (Agent m s e), GlobalEnvironment e)
sendEvent am ge event = (am', ge')
    where
        asLe = map (sendEventMap event ge) (Map.elems am)
        (as, ge') = splitAgentLocalEnvPairs asLe ge
        am' = insertAgents am as

        sendEventMap :: Event m -> GlobalEnvironment e -> (Agent m s e) -> (Agent m s e, LocalEnvironment e)
        sendEventMap event ge a = agentTransformer (a, ge, le) event
            where
                agentTransformer = (trans a)
                le = getLocalEnv a ge

insertAgents :: Map.Map AgentId (Agent m s e) -> [Agent m s e] -> Map.Map AgentId (Agent m s e)
insertAgents am as = foldl (\accMap a -> Map.insert (agentId a) a accMap ) am as

-- NOTE: iteration order makes no difference as they are in fact 'parallel': no agent can see the update of others, all happens at the same time

-- TODO: we could let each agent let collect its own messages by providing the (copy) of all agents
stepAllAgents :: Map.Map AgentId (Agent m s e) -> Double -> GlobalEnvironment e -> (Map.Map AgentId (Agent m s e), GlobalEnvironment e)
stepAllAgents am dt ge = (amDeliveredMsgs, ge')
    where
        (am', ge') = runAgentsParallel dt ge am
        (newAgents, amClearedNewAgents) = collectAndClearNewAgents am'
        amWithNewAgents = insertAgents amClearedNewAgents newAgents
        amDeliveredMsgs = deliverOutMsgs amWithNewAgents
        amRemovedKilled = Map.foldl killAgentFold Map.empty amDeliveredMsgs

runAgentsParallel :: Double -> GlobalEnvironment e -> Map.Map AgentId (Agent m s e) -> (Map.Map AgentId (Agent m s e), GlobalEnvironment e)
runAgentsParallel dt ge am = (am', ge')
    where
        as = Map.elems am
        parAEPairs = parMap rpar (stepAgent dt ge) as -- NOTE: replace by rseq if no hardware-parallelism should be used
        (as', ge') = splitAgentLocalEnvPairs parAEPairs ge
        am' = insertAgents Map.empty as'

-- NOTE: adding agents in front instead of ++: reduces runtime- and memory-overhead extremely
splitAgentLocalEnvPairs :: [(Agent m s e, LocalEnvironment e)] -> GlobalEnvironment e -> ([Agent m s e], GlobalEnvironment e)
splitAgentLocalEnvPairs aeps ge = foldl (\(as, ge') (a, le) -> (a : as, Map.insert (agentId a) le ge') ) ([], ge) aeps

-- TODO: killed agents need to be removed from the environment
killAgentFold :: Map.Map AgentId (Agent m s e) -> Agent m s e -> Map.Map AgentId (Agent m s e)
killAgentFold am a
    | killFlag a = am
    | otherwise = Map.insert (agentId a) a am

-- TODO: new agents need to be added to the environment
collectAndClearNewAgents :: Map.Map AgentId (Agent m s e) -> ([(Agent m s e)], Map.Map AgentId (Agent m s e))
collectAndClearNewAgents am = Map.foldl (\(newAsAcc, amAcc) a -> (newAsAcc ++ (newAgents a), Map.insert (agentId a) a { newAgents = [] } amAcc)) ([], am) am


-- NOTE: this places the messages in the out-box of of the first argument agent at their corresponding receivers in the map
deliverOutMsgs :: Map.Map AgentId (Agent m s e) -> Map.Map AgentId (Agent m s e)
deliverOutMsgs am = foldl (\agentMap' outMsgTup -> deliverMsg agentMap' outMsgTup ) am' allOutMsgs
    where
        (allOutMsgs, am') = collectOutMsgs am

-- NOTE: could be the case that the receiver is no more in the map because it has been killed
deliverMsg :: Map.Map AgentId (Agent m s e) -> (AgentId, AgentId, m) -> Map.Map AgentId (Agent m s e)
deliverMsg am (senderId, receiverId, m)
    | receiverNotFound = am
    | otherwise = Map.insert receiverId a' am
    where
        mayReceiver = Map.lookup receiverId am
        receiverNotFound = isNothing mayReceiver
        a = (fromJust mayReceiver)
        ib = inBox a
        ibM = (senderId, m)
        -- NOTE: need to deliver messages in the received order => append at the end: ib ++ [ibM]
        a' = a { inBox = ib ++ [ibM] }

-- NOTE: first AgentId: senderId, second AgentId: receiverId
collectOutMsgs :: Map.Map AgentId (Agent m s e) -> ([(AgentId, AgentId, m)], Map.Map AgentId (Agent m s e))
collectOutMsgs am = Map.foldl (\(accMsgs, amAcc) a -> ((outBox a) ++ accMsgs, Map.insert (agentId a) a { outBox = [] } amAcc)) ([], am) am



-- PROCESSING THE AGENT
stepAgent :: Double -> GlobalEnvironment e -> Agent m s e -> (Agent m s e, LocalEnvironment e)
stepAgent dt ge a = (aAfterUpdt, leAfterUpdt)
    where
        le = getLocalEnv a ge
        (aAfterMsgProc, leAfterMsgProc) = processAllMessages (a, ge, le)
        agentTransformer = trans aAfterMsgProc
        (aAfterUpdt, leAfterUpdt) = agentTransformer (aAfterMsgProc, ge, leAfterMsgProc) (Dt dt)

processAllMessages :: (Agent m s e, GlobalEnvironment e, LocalEnvironment e) -> (Agent m s e, LocalEnvironment e)
processAllMessages (a, ge, le) = (aAfterMsgs', leAfterMsgs)
    where
        msgs = inBox a
        (aAfterMsgs, leAfterMsgs) = foldl (\(a', le') senderMsgPair -> processMsg (a', ge, le') senderMsgPair) (a, le) msgs
        aAfterMsgs' = aAfterMsgs { inBox = [] }

processMsg :: (Agent m s e, GlobalEnvironment e, LocalEnvironment e) -> (AgentId, m) -> (Agent m s e, LocalEnvironment e)
processMsg (a, ge, le) (senderId, m) = agentTransformer (a, ge, le) (Message (senderId, m))
    where
        agentTransformer = trans a
------------------------------------------------------------------------------------------------------------------------