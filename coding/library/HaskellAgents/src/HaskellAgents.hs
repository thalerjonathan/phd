module HaskellAgents (
    Agent(..),
    AgentId,
    MsgHandler,
    UpdateHandler,
    sendMsg,
    agentsToNeighbourPair,
    addNeighbours,
    stepSimulation,
    createAgent,
    txEnv,
    readEnv,
    writeEnv
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

import Data.Maybe

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
type MsgHandler m s e = (Agent m s e -> m -> AgentId -> STM (Agent m s e))        -- NOTE: need STM to be able to send messages
type UpdateHandler m s e = (Agent m s e -> Double -> STM (Agent m s e))           -- NOTE: need STM to be able to send messages

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
data Agent m s e = Agent {
    agentId :: AgentId,
    mbox :: (TChan (AgentId, m)),
    neighbours :: Map.Map AgentId (TChan (AgentId, m)),        -- NOTE: strength of haskell: ensure by static typing that only neighbours with same message-protocoll
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
        ns = neighbours a
        senderId = agentId a
        targetMbox = Map.lookup targetId ns

createAgent :: AgentId -> s -> MsgHandler m s e -> UpdateHandler m s e -> STM (Agent m s e)
createAgent i s mhdl uhdl = do
                                mb <- newTChan
                                return Agent{ agentId = i,
                                                state = s,
                                                mbox = mb,
                                                neighbours = Map.empty,
                                                msgHandler = mhdl,
                                                updateHandler = uhdl,
                                                env = Nothing }

agentsToNeighbourPair :: [Agent m s e] -> [(AgentId, (TChan (AgentId, m)))]
agentsToNeighbourPair as = map (\a -> (agentId a, mbox a)) as

addNeighbours :: Agent m s e -> [(AgentId, (TChan (AgentId, m)))] -> Agent m s e
addNeighbours a nsPairs = a { neighbours = ns' }
    where
        ns = neighbours a
        ns' = foldl (\acc (k, v) -> Map.insert k v acc ) ns nsPairs

stepSimulation :: [Agent m s e] -> Maybe e -> Double -> Int -> STM ([Agent m s e], Maybe e)
stepSimulation as e dt n = do
                                (asWithEnv, mayEnvVar) <- setEnv as e
                                asFinal <- stepSimulation' asWithEnv dt n
                                if (isNothing mayEnvVar) then
                                    return (asFinal, Nothing)
                                    else
                                        do
                                            e' <- readTVar (fromJust mayEnvVar)
                                            return (asFinal, Just e')

    where
        setEnv :: [Agent m s e] -> Maybe e -> STM ([Agent m s e], Maybe (TVar e))
        setEnv as Nothing = return (as, Nothing)
        setEnv as (Just env) = do
                                envVar <- newTVar env
                                let as' = map (\a -> a { env = Just envVar } ) as
                                return (as', Just envVar)

        stepSimulation' :: [Agent m s e] -> Double -> Int -> STM [Agent m s e]
        stepSimulation' as dt 0 = return as
        stepSimulation' as dt n = do
                                    as' <- mapM (stepAgent dt) as       -- TODO: if running in parallel, then we need to commit at some point using atomically
                                    stepSimulation' as' dt (n-1)

txEnv :: Agent m s e -> (e -> e) -> STM ()
txEnv a tx = do
                let maybeEnv = env a
                if (isNothing maybeEnv) then
                    return ()                               -- NOTE: no environment, do nothing
                    else
                        modifyTVar (fromJust maybeEnv) tx

readEnv :: Agent m s e -> STM e
readEnv a = readTVar (fromJust (env a))

writeEnv :: Agent m s e -> e -> STM ()
writeEnv a e = txEnv a (\_ -> e)

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
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
