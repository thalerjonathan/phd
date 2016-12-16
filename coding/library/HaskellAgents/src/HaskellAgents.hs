module HaskellAgents where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Data.Maybe
import qualified Data.HashMap as Map

-- TODO: implement reading / writing the environment

-- TODO: why not use SF of Yampa as the callback functions? Then we can run all the stuff in Yampa / Dunai
-- TODO: we are running in STM, thats why we cannot run in Yampa, so I need to get into Dunai!

-- TODO: fix parameters which won't change anymore after an Agent has started by using currying

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
    | isNothing targetMbox = return ()                          -- NOTE: receiver not found
    | otherwise = writeTChan (fromJust targetMbox) (senderId, msg)
    where
        ns = neighbours a
        senderId = agentId a
        targetMbox = Map.lookup targetId ns

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

stepAgent :: Double -> Agent m s -> STM (Agent m s)
stepAgent dt a = do
                a' <- processAllMessages a
                let upHdl = updateHandler a'
                a'' <- upHdl a' dt
                return a''
