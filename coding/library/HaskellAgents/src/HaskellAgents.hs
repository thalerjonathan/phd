module HaskellAgents (
    module Control.Monad.STM,
    module Control.Concurrent.STM.TChan,
    module Control.Concurrent.STM.TVar,
    module Data.Maybe,
    Agent(..),
    AgentId,
    MsgHandler,
    UpdateHandler,
    sendMsg,
    sendMsgToRandomNeighbour,
    broadcastMsg,
    updateState,
    agentsToNeighbourPair,
    addNeighbours,
    stepSimulation,
    initStepSimulation,
    advanceSimulation,
    runSimulation,
    createAgent,
    changeEnv,
    readEnv,
    writeEnv
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Maybe

import System.Random

import Debug.Trace

import qualified Data.HashMap as Map

-- TODO: parallelism & concurency
    -- TODO: how can we implement true parallelism? can we use STM somehow or do we need local mailboxes?
    -- TODO: exploit parallelism and concurrency using par monad? problem: STM may rollback and need retry
    -- TODO: is getting rid of STM an option?

-- TODO: Features
    -- TODO: provide implementations for all kinds of sim-teppings but hidden behind a message interface common to all but different semantics with different steppings
    -- TODO: shortcoming: cannot wait blocking for a message so far. utilize yampas event mechanism?

-- TODO: Refactorings
    -- TODO: fix parameters which won't change anymore after an Agent has started by using currying

-- TODO: Yampa/Dunai
    -- TODO: let the whole thing run in Yampa/Dunai so we can leverage the power of the EDSL, SFs, continuations,... of Yampa/Dunai. But because running in STM must use Dunai

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type MsgHandler m s e = (Agent m s e -> m -> AgentId -> STM (Agent m s e))          -- NOTE: need STM to be able to send messages
type UpdateHandler m s e = (Agent m s e -> Double -> STM (Agent m s e))             -- NOTE: need STM to be able to send messages
type OutFunc m s e = (([Agent m s e], Maybe e) -> IO (Bool, Double))

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
data Agent m s e = Agent {
    agentId :: AgentId,
    mbox :: (TChan (AgentId, m)),
    neighbourIds :: [AgentId],
    neighbourMbox :: Map.Map AgentId (TChan (AgentId, m)),        -- NOTE: strength of haskell: ensure by static typing that only neighbours with same message-protocoll
    msgHandler :: MsgHandler m s e,
    updateHandler :: UpdateHandler m s e,
    state :: s,
    env :: Maybe (TVar e)                                      -- NOTE: environment is optional
}

sendMsg :: Agent m s e -> m -> AgentId -> STM ()
sendMsg a msg targetId
    | isNothing targetMbox = return ()                          -- NOTE: receiver not found in the neighbours
    | otherwise = writeTChan (fromJust targetMbox) (senderId, msg)
    where
        nsBox = neighbourMbox a
        senderId = agentId a
        targetMbox = Map.lookup targetId nsBox

broadcastMsg :: Agent m s e -> m -> STM ()
broadcastMsg a msg = do
                        actions <- (trace "broadcast1 " mapM (sendMsg a msg) nsIds)
                        trace "broadcast" return ()

    where
        nsIds = neighbourIds a

{-
broadcastMsg :: Agent m s e -> m -> STM ()
broadcastMsg a msg = do
                        actions <- mapM (\mbox -> writeTChan mbox (senderId, msg)) nsBox
                        trace "broadcast" return ()

    where
        nsBox = neighbourMbox a
        senderId = agentId a
-}

sendMsgToRandomNeighbour :: (RandomGen g) => Agent m s e -> m -> g -> STM g
sendMsgToRandomNeighbour a msg g = do
                                    sendMsg a msg randAgentId
                                    return g'
                                        where
                                            s = state a
                                            nIds = neighbourIds a
                                            nsCount = length nIds
                                            (randIdx, g') = randomR(0, nsCount-1) g
                                            randAgentId = nIds !! randIdx

updateState :: Agent m s e -> (s -> s) -> Agent m s e
updateState a sf = a { state = s' }
    where
        s = state a
        s' = sf s

createAgent :: AgentId -> s -> MsgHandler m s e -> UpdateHandler m s e -> STM (Agent m s e)
createAgent i s mhdl uhdl = do
                                mb <- newTChan
                                return Agent{ agentId = i,
                                                state = s,
                                                mbox = mb,
                                                neighbourMbox = Map.empty,
                                                neighbourIds = [],
                                                msgHandler = mhdl,
                                                updateHandler = uhdl,
                                                env = Nothing }

agentsToNeighbourPair :: [Agent m s e] -> [(AgentId, (TChan (AgentId, m)))]
agentsToNeighbourPair as = Prelude.map (\a -> (agentId a, mbox a)) as

addNeighbours :: Agent m s e -> [(AgentId, (TChan (AgentId, m)))] -> Agent m s e
addNeighbours a nsPairs = a { neighbourMbox = nsMbox', neighbourIds = nsIds' }
    where
        nsMbox = neighbourMbox a
        nsMbox' = foldl (\acc (k, v) -> Map.insert k v acc ) nsMbox nsPairs
        nsIds = neighbourIds a
        nsIds' = foldl (\acc (k, v) -> nsIds ++ [k] ) nsIds nsPairs

changeEnv :: Agent m s e -> (e -> e) -> STM ()
changeEnv a tx = do
                    let maybeEnv = env a
                    if (isNothing maybeEnv) then
                        return ()                               -- NOTE: no environment, do nothing
                        else
                            modifyTVar (fromJust maybeEnv) tx

readEnv :: Agent m s e -> STM e
readEnv a = readTVar (fromJust (env a))

writeEnv :: Agent m s e -> e -> STM ()
writeEnv a e = changeEnv a (\_ -> e)

-- TODO: return all steps of agents and environment
stepSimulation :: [Agent m s e] -> Maybe e -> Double -> Int -> STM ([Agent m s e], Maybe e)
stepSimulation as e dt n = do
                                (asWithEnv, mayEnvVar) <- setEnv as e
                                asFinal <- stepSimulation' asWithEnv dt n
                                finalEnv <- maybeEnvVarToMaybeEnv mayEnvVar
                                return (asFinal, finalEnv)
    where
        stepSimulation' :: [Agent m s e] -> Double -> Int -> STM [Agent m s e]
        stepSimulation' as dt 0 = return as
        stepSimulation' as dt n = do
                                    as' <- mapM (stepAgent dt) as       -- TODO: if running in parallel, then we need to commit at some point using atomically
                                    stepSimulation' as' dt (n-1)


initStepSimulation :: [Agent m s e] -> Maybe e -> STM [Agent m s e]
initStepSimulation as e = do
                            (asWithEnv, mayEnvVar) <- setEnv as e
                            return asWithEnv

advanceSimulation :: [Agent m s e] -> Double -> STM ([Agent m s e], Maybe e)
advanceSimulation as dt = do
                            as' <- mapM (stepAgent dt) as
                            let a' = head as'                           -- TODO: this is a dirty hack, replace by original TVar when having some SimulationHandle
                            let mayEnvVar = env a'
                            env' <- maybeEnvVarToMaybeEnv mayEnvVar
                            return (as', env')

runSimulation :: [Agent m s e] -> Maybe e -> OutFunc m s e -> IO ()
runSimulation as e out = do
                            (asWithEnv, mayEnvVar) <- atomically $ setEnv as e
                            runSimulation' asWithEnv 0.0 mayEnvVar out

        where
            runSimulation' :: [Agent m s e] -> Double -> Maybe (TVar e) -> OutFunc m s e -> IO ()
            runSimulation' as dt mayEnvVar out = do
                                        as' <- atomically $ mapM (stepAgent dt) as
                                        finalEnv <- atomically $ maybeEnvVarToMaybeEnv mayEnvVar
                                        (cont, dt') <- out (as', finalEnv)
                                        if cont == True then
                                            runSimulation' as' dt' mayEnvVar out
                                            else
                                                return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
maybeEnvVarToMaybeEnv :: Maybe (TVar e) -> STM (Maybe e)
maybeEnvVarToMaybeEnv Nothing = return Nothing
maybeEnvVarToMaybeEnv (Just var) = do
                                    e <- readTVar var
                                    return (Just e)

setEnv :: [Agent m s e] -> Maybe e -> STM ([Agent m s e], Maybe (TVar e))
setEnv as Nothing = return (as, Nothing)
setEnv as (Just env) = do
                        envVar <- newTVar env
                        let as' = Prelude.map (\a -> a { env = Just envVar } ) as
                        return (as', Just envVar)


receiveMsg :: Agent m s e -> STM (Maybe (AgentId, m))
receiveMsg a = tryReadTChan mb
    where
        mb = mbox a

processMsg :: Agent m s e -> (AgentId, m) -> STM (Agent m s e)
processMsg a (senderId, msg) = handler a msg senderId
    where
        handler = msgHandler a

processAllMessages :: Agent m s e -> STM (Agent m s e)
processAllMessages a = do
                        msg <- receiveMsg a
                        if ( isNothing msg ) then
                            return a
                                else
                                    do
                                        a' <- processMsg a (fromJust msg)
                                        processAllMessages a'

-- TODO: need to add atomically when in parallel
-- TODO: process messages first or update first? this has different semantics when messages are seen immediately
stepAgent :: Double -> Agent m s e -> STM (Agent m s e)
stepAgent dt a = do
                a' <- processAllMessages a
                let upHdl = updateHandler a'
                a'' <- upHdl a' dt
                return a''
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
