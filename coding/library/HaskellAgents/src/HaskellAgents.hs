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
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Data.Maybe
import qualified Data.HashMap as Map

-- NOTE: this is what I want to do with this library:
-- take haskell, add yampa and dunai and implement ActorModel on top using STM => have an ABM library in Haskell, put on hackage. NO IO!

-- TODO: is getting rid of STM an option?

-- TODO: provide implementations for all kinds of sim-teppings but hidden behind a message interface common to all but different semantics with different steppings

-- TODO: how can we implement true parallelism? can we use STM somehow or do we need local mailboxes?

-- TODO: add generic way of read/write to an environment e

-- TODO: exploit parallelism and concurrency using par monad? problem: STM may rollback and need retry

-- TODO: let the whole thing run in Yampa/Dunai so we can leverage the power of the EDSL, SFs, continuations,... of Yampa/Dunai. But because running in STM must use Dunai

-- TODO: shortcoming: cannot wait blocking for a message so far. utilize yampas event mechanism?

-- TODO: fix parameters which won't change anymore after an Agent has started by using currying



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

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type MsgHandler m s = (Agent m s -> m -> AgentId -> STM (Agent m s))        -- NOTE: need STM to be able to send messages
type UpdateHandler m s = (Agent m s -> Double -> STM (Agent m s))           -- NOTE: need STM to be able to send messages

{- NOTE:    m is the type of messages the agent understands
            s is a generic state of the agent e.g. any data
            TODO:
                e will be the type of environment the agent acts upon
-}
data Agent m s = Agent {
    agentId :: AgentId,
    mbox :: (TChan (AgentId, m)),
    neighbours :: Map.Map AgentId (TChan (AgentId, m)),        -- NOTE: strength of haskell: ensure by static typing that only neighbours with same message-protocoll
    msgHandler :: MsgHandler m s,
    updateHandler :: UpdateHandler m s,
    state :: s
}

sendMsg :: Agent m s -> m -> AgentId -> STM ()
sendMsg a msg targetId
    | isNothing targetMbox = return ()                          -- NOTE: receiver not found in the neighbours
    | otherwise = writeTChan (fromJust targetMbox) (senderId, msg)
    where
        ns = neighbours a
        senderId = agentId a
        targetMbox = Map.lookup targetId ns

createAgent :: AgentId -> s -> MsgHandler m s -> UpdateHandler m s -> STM (Agent m s)
createAgent i s mhdl uhdl = do
                                mb <- newTChan
                                return Agent{ agentId = i,
                                                state = s,
                                                mbox = mb,
                                                neighbours = Map.empty,
                                                msgHandler = mhdl,
                                                updateHandler = uhdl }

agentsToNeighbourPair :: [Agent m s] -> [(AgentId, (TChan (AgentId, m)))]
agentsToNeighbourPair as = map (\a -> (agentId a, mbox a)) as

addNeighbours :: Agent m s -> [(AgentId, (TChan (AgentId, m)))] -> Agent m s
addNeighbours a nsPairs = a { neighbours = ns' }
    where
        ns = neighbours a
        ns' = foldl (\acc (k, v) -> Map.insert k v acc ) ns nsPairs

stepSimulation :: [Agent m s] -> Double -> STM [Agent m s]
stepSimulation as dt = mapM (stepAgent dt) as


------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
receiveMsg :: Agent m s -> STM (Maybe (AgentId, m))
receiveMsg a = tryReadTChan mb
    where
        mb = mbox a

processMsg :: Agent m s -> (AgentId, m) -> STM (Agent m s)
processMsg a (senderId, msg) = handler a msg senderId
    where
        handler = msgHandler a

processAllMessages :: Agent m s -> STM (Agent m s)
processAllMessages a = do
                        msg <- receiveMsg a
                        if ( isNothing msg ) then
                            return a
                                else
                                    do
                                        a' <- processMsg a (fromJust msg)
                                        processAllMessages a'

stepAgent :: Double -> Agent m s -> STM (Agent m s)
stepAgent dt a = do
                a' <- processAllMessages a
                let upHdl = updateHandler a'
                a'' <- upHdl a' dt
                return a''
------------------------------------------------------------------------------------------------------------------------