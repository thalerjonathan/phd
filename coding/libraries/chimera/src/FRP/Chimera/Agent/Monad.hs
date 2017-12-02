{-# LANGUAGE Arrows #-}
module FRP.Chimera.Agent.Monad 
  (
    AgentMonadicBehaviour
  , AgentMonadicBehaviourReadEnv
  , AgentMonadicBehaviourNoEnv

  , createAgentM
  , killM
  , isDeadM

  , sendMessageM
  , sendMessageToM
  , sendMessagesM
  , broadcastMessageM
  , onMessageM
  , onMessageMState

  , conversationM
  , conversationEndM
  , conversationReplyMonadicRunner
  , conversationIgnoreEnvReplyMonadicRunner
  , conversationIgnoreReplyMonadicRunner
  , conversationIgnoreReplyMonadicRunner'

  , bypassEnvironment

  , updateAgentStateM
  , agentStateM
  , setAgentStateM
  , agentStateFieldM

  , agentMonadic
  , agentMonadicReadEnv
  , agentMonadicIgnoreEnv

  , ifThenElse
  , ifThenElseM
  ) where

import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe

import FRP.Yampa

import FRP.Chimera.Agent.Agent

type AgentMonadicBehaviour s m e          = e -> Double -> AgentIn s m e -> State (AgentOut s m e) e
type AgentMonadicBehaviourReadEnv s m e   = e -> Double -> AgentIn s m e -> State (AgentOut s m e) ()
type AgentMonadicBehaviourNoEnv s m e     = Double -> AgentIn s m e -> State (AgentOut s m e) ()

-------------------------------------------------------------------------------
-- GENERAL
-------------------------------------------------------------------------------
createAgentM :: AgentDef s m e -> State (AgentOut s m e) ()
createAgentM newDef = state (\ao -> ((),createAgent newDef ao))

killM :: State (AgentOut s m e) ()
killM = state (\ao -> ((), kill ao))

isDeadM :: State (AgentOut s m e) Bool
isDeadM = state (\ao -> (isDead ao, ao))
  
-------------------------------------------------------------------------------
-- MESSAGING
-------------------------------------------------------------------------------
sendMessageM :: AgentMessage m -> State (AgentOut s m e) ()
sendMessageM msg = state (\ao -> ((), sendMessage msg ao))

sendMessageToM :: AgentId -> m -> State (AgentOut s m e) ()
sendMessageToM receiver msg = state (\ao -> ((), sendMessageTo receiver msg ao))

sendMessagesM :: [AgentMessage m] -> State (AgentOut s m e) ()
sendMessagesM msgs = state (\ao -> ((), sendMessages msgs ao))

broadcastMessageM :: m -> [AgentId] -> State (AgentOut s m e) ()
broadcastMessageM m receiverIds = state (broadcastMessageMAux m)
  where
    broadcastMessageMAux :: m -> AgentOut s m e -> ((), AgentOut s m e)
    broadcastMessageMAux m ao = ((), sendMessages msgs ao)
      where
        n = length receiverIds
        ms = replicate n m
        msgs = zip receiverIds ms

onMessageMState :: (AgentMessage m -> State acc ()) -> AgentIn s m e -> State acc ()
onMessageMState msgHdl ai = onMessageM (\_ msg -> msgHdl msg) ai ()

onMessageM :: (Monad mon) => (acc -> AgentMessage m -> mon acc) -> AgentIn s m e -> acc -> mon acc
onMessageM msgHdl ai acc
    | not hasMessages = return acc
    -- | otherwise = foldM (\acc msg -> msgHdl acc msg) acc msgs
    | otherwise = foldM msgHdl acc msgs
  where
    msgsEvt = aiMessages ai
    hasMessages = isEvent msgsEvt
    msgs = fromEvent msgsEvt

-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming state isJust
agentStateM :: State (AgentOut s m e) s
agentStateM = get >>= (\ao -> return $ fromJust $ aoState ao)

setAgentStateM :: s -> State (AgentOut s m e) ()
setAgentStateM s = state (\ao -> ((), setAgentState s ao))

-- NOTE: assuming state isJust
updateAgentStateM :: (s -> s) -> State (AgentOut s m e) ()
updateAgentStateM f = state (updateAgentStateMAux f)
  where
    updateAgentStateMAux :: (s -> s) 
                        -> AgentOut s m e 
                        -> ((), AgentOut s m e)
    updateAgentStateMAux f ao = ((), updateAgentState f ao)

-- NOTE: assuming state isJust
agentStateFieldM :: (s -> t) -> State (AgentOut s m e) t
agentStateFieldM f = state (agentStateFieldMAux f)
  where
    agentStateFieldMAux :: (s -> t) 
                        -> AgentOut s m e
                        -> (t, AgentOut s m e)
    agentStateFieldMAux f ao = (f s, ao)
      where
        s = fromJust $ aoState ao

-------------------------------------------------------------------------------
-- CONVERSATIONS
-------------------------------------------------------------------------------
conversationM :: AgentMessage m
                -> AgentConversationSender s m e
                -> State (AgentOut s m e) ()
conversationM msg replyHdl = state (\ao -> ((), conversation msg replyHdl ao))

conversationEndM :: State (AgentOut s m e) ()
conversationEndM = state (\ao -> ((), conversationEnd ao))

conversationReplyMonadicRunner :: (Maybe (AgentMessage m) -> e -> State (AgentOut s m e) e) -> AgentConversationSender s m e
conversationReplyMonadicRunner replyAction ao e mayReply = (ao', e')
  where
    (e', ao') = runState (replyAction mayReply e) ao

conversationIgnoreEnvReplyMonadicRunner :: (Maybe (AgentMessage m) -> State (AgentOut s m e) ()) -> AgentConversationSender s m e
conversationIgnoreEnvReplyMonadicRunner replyAction ao e mayReply = (ao', e)
  where
    (_, ao') = runState (replyAction mayReply) ao

-- NOTE: when ignoring the reply it makes also sense to bypass the environment
conversationIgnoreReplyMonadicRunner :: State (AgentOut s m e) () -> AgentConversationSender s m e
conversationIgnoreReplyMonadicRunner replyAction ao e  _ = (ao', e)
  where
    (_, ao') = runState replyAction ao

-- NOTE: for the case one does not want to bypass the environment
conversationIgnoreReplyMonadicRunner' :: (e -> State (AgentOut s m e) e) -> AgentConversationSender s m e
conversationIgnoreReplyMonadicRunner' replyAction ao e _ = (ao', e')
  where
    (e', ao') = runState (replyAction e) ao

bypassEnvironment :: State (AgentOut s m e) () -> e -> State (AgentOut s m e) e
bypassEnvironment a e = a >> return e

-------------------------------------------------------------------------------
-- MONADIC WRAPPERS
-------------------------------------------------------------------------------
agentMonadic :: AgentMonadicBehaviour s m e -> AgentOut s m e -> AgentBehaviour s m e
agentMonadic f ao = proc (ain, e) -> do
  age <- time -< ()
  let (e', ao') = runState (f e age ain) ao
  returnA -< (ao', e')

agentMonadicReadEnv :: AgentMonadicBehaviourReadEnv s m e -> AgentOut s m e -> AgentBehaviour s m e
agentMonadicReadEnv f ao = proc (ain, e) -> do
  age <- time -< ()
  let ao' = execState (f e age ain) ao
  returnA -< (ao', e)

agentMonadicIgnoreEnv :: AgentMonadicBehaviourNoEnv s m e -> AgentOut s m e -> AgentBehaviour s m e
agentMonadicIgnoreEnv f ao = proc (ain, e) -> do
  age <- time -< ()
  let ao' = execState (f age ain) ao
  returnA -< (ao', e)

-------------------------------------------------------------------------------
-- MONADIC UTILITIES
-------------------------------------------------------------------------------
ifThenElse :: Monad m => Bool -> m a -> m a -> m a
ifThenElse p trueAction falseAction = if p then trueAction else falseAction

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM test trueAction falseAction = test >>= \t -> if t then trueAction else falseAction
