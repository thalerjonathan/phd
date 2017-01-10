{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module PureAgentsPar (
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
    addNeighbours,
    stepSimulation,
    initStepSimulation,
    advanceSimulation,
    runSimulation,
    createAgent,
    updateEnv,
    readEnv,
    writeEnv,
    extractEnv,
    extractAgents
  ) where

import Data.Maybe
import System.Random

import Control.DeepSeq
import GHC.Generics (Generic)
import Control.Monad.Par

import qualified Data.HashMap as Map

-- TODO: BIG problem: how do we treat the environment now as every agent has it's local copy => only local updates
    -- TODO: this would allow us the easily calculate all possible solutions e.g. in a discrete case (but infeasible if big)

-- TODO: Yampa/Dunai
    -- TODO: let the whole thing run in Yampa/Dunai so we can leverage the power of the EDSL, SFs, continuations,... of Yampa/Dunai. But because running in STM must use Dunai
    -- TODO: implement wait blocking for a message so far. utilize yampas event mechanism?

-- TODO: build a monad to chain actions of the agent and always run inside an agent-monad
    -- TODO: then we again need dunai

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
-- The super-set of all Agent-Messages
data Msg m = Dt Double | Domain m
-- An agent-message is always a tuple of a message with the sender-id
type AgentMessage m = (AgentId, Msg m)

type AgentTransformer m s e = (Agent m s e -> AgentMessage m -> Agent m s e)
type OutFunc m s e = (([Agent m s e], Maybe e) -> IO (Bool, Double))

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
    newAgents :: [Agent m s e],
    env :: Maybe e                                      -- NOTE: environment is optional
} deriving (Generic, NFData)

data SimHandle m s e = SimHandle {
    simHdlAgents :: [Agent m s e],
    simHdlEnv :: Maybe e
}

newAgent :: Agent m s e -> Agent m s e -> Agent m s e
newAgent aParent aNew = aParent { newAgents = nas ++ [aNew'] }
    where
        aNew' = aNew { env = (env aParent)} -- NOTE: set to same environment
        nas = newAgents aParent

kill :: Agent m s e -> Agent m s e
kill a = a { killFlag = True }

sendMsg :: Agent m s e -> m -> AgentId -> Agent m s e
sendMsg a m targetId
    | targetNotFound = a                                     -- NOTE: receiver not found in the neighbours
    | otherwise = a { outBox = newOutBox }
    where
        senderId = agentId a
        targetNotFound = isNothing (Map.lookup targetId (neighbours a))       -- NOTE: do we really need to look-up the neighbours?
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

createAgent :: AgentId -> s -> AgentTransformer m s e -> Agent m s e
createAgent i s t = Agent{ agentId = i,
                                    state = s,
                                    outBox = [],
                                    inBox = [],
                                    killFlag = False,
                                    neighbours = Map.empty,
                                    newAgents = [],
                                    trans = t,
                                    env = Nothing }

addNeighbours :: Agent m s e -> [Agent m s e] -> Agent m s e
addNeighbours a ns = a { neighbours = newNeighbours }
    where
        newNeighbours = foldl (\acc a -> Map.insert (agentId a) (agentId a) acc) (neighbours a) ns

updateEnv :: Agent m s e -> (e -> e) -> Agent m s e
updateEnv a tx
    | isNothing maybeEnv = a
    | otherwise = a { env = modifiedEnv }
        where
            maybeEnv = env a
            modifiedEnv = Just (tx (fromJust maybeEnv))

readEnv :: Agent m s e -> e
readEnv = fromJust . env

writeEnv :: Agent m s e -> e -> Agent m s e
writeEnv a e = updateEnv a (\_ -> e)

extractEnv :: SimHandle m s e -> Maybe e
extractEnv = simHdlEnv

extractAgents :: SimHandle m s e -> [Agent m s e]
extractAgents = simHdlAgents

-- TODO: return all steps of agents and environment
stepSimulation :: (NFData m, NFData s, NFData e) => [Agent m s e] -> Maybe e -> Double -> Int -> ([Agent m s e], Maybe e)
stepSimulation as e dt n = (asFinal, finalEnv)
    where
        (asWithEnv, mayEnvVar) = setEnv as e
        asFinal = stepSimulation' asWithEnv dt n
        finalEnv = mayEnvVar        -- TODO: handle env-problem: it should be global but is always agent-local

        stepSimulation' :: (NFData m, NFData s, NFData e) => [Agent m s e] -> Double -> Int -> [Agent m s e]
        stepSimulation' as dt 0 = as
        stepSimulation' as dt n = stepSimulation' as' dt (n-1)
            where
                as' = stepAllAgents as dt

initStepSimulation :: (NFData m, NFData s, NFData e) => [Agent m s e] -> Maybe e -> ([Agent m s e], SimHandle m s e)
initStepSimulation as e = (asWithEnv, hdl)
    where
        (asWithEnv, mayEnvVar) = setEnv as e
        hdl = SimHandle { simHdlAgents = asWithEnv, simHdlEnv = mayEnvVar }

advanceSimulation :: (NFData m, NFData s, NFData e) => SimHandle m s e -> Double -> ([Agent m s e], Maybe e, SimHandle m s e)
advanceSimulation hdl dt = (as', env', hdl')
    where
        as = simHdlAgents hdl
        as' = stepAllAgents as dt
        env' = extractEnv hdl
        hdl' = hdl { simHdlAgents = as' }         -- TODO: handle env-problem: it should be global but is always agent-local

runSimulation :: (NFData m, NFData s, NFData e) => [Agent m s e] -> Maybe e -> OutFunc m s e -> IO ()
runSimulation as e out = runSimulation' asWithEnv 0.0 mayEnvVar out
    where
        (asWithEnv, mayEnvVar) = setEnv as e

        runSimulation' :: (NFData m, NFData s, NFData e) => [Agent m s e] -> Double -> Maybe e -> OutFunc m s e -> IO ()
        runSimulation' as dt mayEnvVar out = do
                                                let as' = stepAllAgents as dt
                                                let finalEnv = mayEnvVar    -- TODO: handle env-problem: it should be global but is always agent-local
                                                (cont, dt') <- out (as', finalEnv)
                                                if cont == True then
                                                    runSimulation' as' dt' mayEnvVar out
                                                    else
                                                        return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
-- TODO: handle env-problem: it should be global but is always agent-local
setEnv :: [Agent m s e] -> Maybe e -> ([Agent m s e], Maybe e)
setEnv as Nothing = (as, Nothing)
setEnv as (Just env) = (as', Just env)
    where
        as' = Prelude.map (\a -> a { env = Just env } ) as   -- TODO: handle env-problem: it should be global but is always agent-local

-- TODO: can we parallelize this?
stepAllAgents :: [Agent m s e] -> Double -> [Agent m s e]
stepAllAgents as dt = deliverOutMsgs steppedAs
    where
        steppedAs = foldl (stepAllAgentsFold dt) [] as

        stepAllAgentsFold :: Double -> [Agent m s e] -> Agent m s e -> [Agent m s e]
        stepAllAgentsFold dt as a
            | killAgent = (as ++ nas)
            | otherwise = (as ++ [aNewAgentsRemoved] ++ nas)
            where
                aAfterStep = stepAgent dt a
                nas = newAgents aAfterStep
                killAgent = (killFlag aAfterStep)
                aNewAgentsRemoved = aAfterStep { newAgents = [] }

-- TODO: can we parallelize this?
deliverOutMsgs :: [Agent m s e] -> [Agent m s e]
deliverOutMsgs as = Map.elems agentsWithMsgs
    where
        (allOutMsgs, as') = collectOutMsgs as
        agentMap = foldl (\acc a -> Map.insert (agentId a) a acc ) Map.empty as'        -- TODO: hashmap will change the order, is this alright? should be when working in parallel
        agentsWithMsgs = foldl (\agentMap' outMsgTup -> deliverMsg agentMap' outMsgTup ) agentMap allOutMsgs

deliverMsg :: Map.Map AgentId (Agent m s e) -> (AgentId, AgentId, m) -> Map.Map AgentId (Agent m s e)
deliverMsg agentMap (senderId, targetId, m) = Map.insert targetId a' agentMap
    where
        a = fromJust (Map.lookup targetId agentMap)    -- NOTE: must be in the map
        ib = inBox a
        ibM = (senderId, m)
        a' = a { inBox = ib ++ [ibM] }     -- TODO: is ordering important??? what is the meaning of ordering in this case?

-- NOTE: first AgentId: senderId, second AgentId: targetId
collectOutMsgs :: [Agent m s e] -> ([(AgentId, AgentId, m)], [Agent m s e])
collectOutMsgs as = foldl (\(accMsgs, accAs) a -> ((outBox a) ++ accMsgs, (a { outBox = [] }) : accAs) ) ([], []) as

-- TODO: run parallel
stepAgent :: Double -> Agent m s e -> Agent m s e
stepAgent dt a = aAfterUpdt
    where
        aAfterMsgProc = processAllMessages a
        aAfterUpdt = (trans aAfterMsgProc) aAfterMsgProc (-1, Dt dt)

processMsg :: Agent m s e -> (AgentId, m) -> Agent m s e
processMsg a (senderId, m) = (trans a) a (senderId, Domain m)

processAllMessages :: Agent m s e -> Agent m s e
processAllMessages a = aAfterMsgs { inBox = [] }
    where
        aAfterMsgs = foldl (\a' senderMsgPair -> processMsg a' senderMsgPair) a (inBox a)
------------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------------
{- TODO: experiment with 'active' messages which represent a conversation between two agents but will be executed only by
         the receiving agent and will result in a final result which is then local to the receiving agent. This allows
          to encapsulate data in a more general way through closures and to have some logic working upon the data. -}
{-
main :: IO ()
main = do
    let b = makeOffer bid
    let r = executeOffer ask b
    putStrLn (show r)
        where
            bid = Bid 42
            ask = Ask 41

data Offering a = Bid a | Ask a | Accept | Refuse deriving (Show)

data Conversation m = Msg m | Conv (m -> Conversation m)

{- NOTE: this part sends an offer enclosed in the lambda and will compare it to a passed in offer where a reply is then made
         TODO: introduce additional step> send o along with an additional reply and wait
-}
makeOffer :: Offering Int -> Conversation (Offering Int)
makeOffer o = Conv (\o' -> if compareOffer o o' then
                                Msg Accept
                                    else
                                        Msg Refuse )

{- NOTE: this part is the counterpart which allows to execute a given offering-conversation with a given offering
         problem: we loose track were we are like in makeOffer
         TODO: must always terminate and finally return Message

-}
executeOffer :: Offering Int -> Conversation (Offering Int) -> Offering Int
executeOffer o (Conv f) = executeOffer o (f o)
executeOffer o (Msg o') = o'

compareOffer :: Offering Int -> Offering Int -> Bool
compareOffer (Bid a) (Ask a') = a >= a'
compareOffer (Ask a) (Bid a') = a <= a'
compareOffer _ _ = False
-}
-----------------------------------------------------------------------------------------------------------------------------
