{-# LANGUAGE Arrows #-}
module FRP.Chimera.Agent.Agent 
  (
    AgentId
  , AgentMessage
  , AgentBehaviour

  , AgentConversationSender

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)

  , AgentObservable

  , AgentPureBehaviour
  , AgentPureBehaviourReadEnv
  , AgentPureBehaviourNoEnv

  , agentId
  , createAgent
  , kill
  , isDead
  , agentOut
  , agentOutObs
  
  , startingAgentIn
  , startingAgent
  , startingAgentInFromAgentDef

  , sendMessage
  , sendMessageTo
  , sendMessages
  , broadcastMessage
  , hasMessage
  , onMessage
  , onFilterMessage
  , onMessageFrom
  , onMessageType

  , hasConversation
  , conversation
  , conversationEnd

  , agentState
  , updateAgentState
  , setAgentState

  , nextAgentId

  , onStart
  , onEvent

  , recInitAllowed
  , allowsRecOthers
  , recursive
  , unrecursive
  , isRecursive
  , agentRecursions

  , mergeMessages

  , agentPure
  , agentPureReadEnv
  , agentPureIgnoreEnv
  ) where

import Control.Concurrent.STM.TVar
import Data.List
import Data.Maybe

import FRP.BearRiver

import FRP.Chimera.Simulation.Internal

type AgentId                = Int
type AgentMessage m         = (AgentId, m)
type AgentBehaviour s m e   = SF (StateT (AgentOut s m e) (Rand g)) (AgentIn s m e, e) e
type MessageFilter m        = AgentMessage m -> Bool
type AgentObservable s      = (AgentId, s)

{--
type AgentConversationReceiver s m e = (AgentIn s m e
                                            -> e
                                            -> AgentMessage m
                                            -> Maybe (s, m, e)) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging
-}

type AgentConversationSender s m e = (AgentOut s m e
                                        -> e
                                        -> Maybe (AgentMessage m)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> (AgentOut s m e, e))

type AgentPureBehaviour s m e         = e -> Double -> AgentIn s m e -> (AgentOut s m e, e)
type AgentPureBehaviourReadEnv s m e  = e -> Double -> AgentIn s m e -> AgentOut s m e
type AgentPureBehaviourNoEnv s m e    = Double -> AgentIn s m e -> AgentOut s m e

data AgentDef s m e = AgentDef
  { adId              :: !AgentId
  , adBeh             :: AgentBehaviour s m e
  , adInitMessages    :: !(Event [AgentMessage m])     -- AgentId identifies sender
  }

data AgentIn s m e = AgentIn 
  { aiId                    :: !AgentId
  , aiMessages              :: !(Event [AgentMessage m])     -- AgentId identifies sender
  , aiConversationIncoming  :: !(Event (AgentMessage m))
  , aiStart                 :: !(Event ())
  , aiRec                   :: !(Event [(AgentObservable s, e)])
  , aiRecInitAllowed        :: !Bool
  , aiIdGen                 :: !(TVar Int)
  }

data AgentOut s m e = AgentOut 
  { aoKill                  :: !(Event ())
  , aoCreate                :: !(Event [AgentDef s m e])
  , aoMessages              :: !(Event [AgentMessage m])     -- AgentId identifies receiver
  , aoConversationRequest   :: !(Event (AgentMessage m, AgentConversationSender s m e))
  , aoConversationReply     :: !(Event (AgentMessage m))
  , aoState                 :: !(Maybe s) -- OPTIONAL observable state
  , aoRec                   :: !(Event ())
  , aoRecOthersAllowed      :: !Bool
  }

-------------------------------------------------------------------------------
-- GENERAL 
-------------------------------------------------------------------------------
agentId :: AgentIn s m e -> AgentId
agentId = aiId 

createAgent :: AgentDef s m e -> AgentOut s m e -> AgentOut s m e
createAgent newDef ao = ao { aoCreate = createEvt }
  where
    oldCreateEvt = aoCreate ao
    createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

agentOut:: AgentOut s m e
agentOut = agentOutAux Nothing

agentOutObs :: s -> AgentOut s m e
agentOutObs s = agentOutAux (Just s)

nextAgentId :: AgentIn s m e -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut s m e -> AgentOut s m e
kill ao = ao { aoKill = Event () }

isDead :: AgentOut s m e -> Bool
isDead = isEvent . aoKill

-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------
onStart :: (AgentOut s m e -> AgentOut s m e) -> AgentIn s m e -> AgentOut s m e -> AgentOut s m e
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
  where
    startEvt = aiStart ai

onEvent :: (AgentOut s m e -> AgentOut s m e) -> Event () -> AgentOut s m e -> AgentOut s m e
onEvent evtHdl evt ao = event ao (\_ -> evtHdl ao) evt

-------------------------------------------------------------------------------
-- MESSAGING
-------------------------------------------------------------------------------
sendMessage :: AgentMessage m -> AgentOut s m e -> AgentOut s m e
sendMessage msg ao = ao { aoMessages = mergedMsgs }
  where
    newMsgEvent = Event [msg]
    existingMsgEvent = aoMessages ao
    mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

sendMessageTo :: AgentId -> m -> AgentOut s m e -> AgentOut s m e
sendMessageTo aid msg ao = sendMessage (aid, msg) ao

sendMessages :: [AgentMessage m] -> AgentOut s m e ->  AgentOut s m e
sendMessages msgs ao = foldr sendMessage ao msgs

broadcastMessage :: m -> [AgentId] -> AgentOut s m e -> AgentOut s m e
broadcastMessage m receiverIds ao = sendMessages msgs ao
  where
    n = length receiverIds
    ms = replicate n m
    msgs = zip receiverIds ms

hasMessage :: (Eq m) => m -> AgentIn s m e -> Bool
hasMessage m ai
    | not hasAnyMessage = False
    | otherwise = hasMsg
  where
    msgsEvt = aiMessages ai
    hasAnyMessage = isEvent msgsEvt
    msgs = fromEvent msgsEvt
    hasMsg = Data.List.any ((==m) . snd) msgs

onMessage :: (AgentMessage m -> acc -> acc) -> AgentIn s m e -> acc -> acc
onMessage msgHdl ai a 
    | not hasMessages = a
    | otherwise = foldr (\msg acc'-> msgHdl msg acc') a msgs
  where
    msgsEvt = aiMessages ai
    hasMessages = isEvent msgsEvt
    msgs = fromEvent msgsEvt

onFilterMessage :: MessageFilter m -> (AgentMessage m -> acc -> acc) -> AgentIn s m e -> acc -> acc
onFilterMessage msgFilter msgHdl ai acc
    | not hasMessages = acc
    | otherwise = foldr (\msg acc'-> msgHdl msg acc') acc filteredMsgs
  where
    msgsEvt = aiMessages ai
    hasMessages = isEvent msgsEvt
    msgs = fromEvent msgsEvt
    filteredMsgs = filter msgFilter msgs

onMessageFrom :: AgentId -> (AgentMessage m -> acc -> acc) -> AgentIn s m e -> acc -> acc
onMessageFrom senderId msgHdl ai acc = onFilterMessage filterBySender msgHdl ai acc
  where
    filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> (AgentMessage m -> acc -> acc) -> AgentIn s m e -> acc -> acc
onMessageType m msgHdl ai acc = onFilterMessage filterByMsgType msgHdl ai acc
    where
        filterByMsgType = (==m) . snd 

-------------------------------------------------------------------------------
-- OBSERVABLE STATE
-------------------------------------------------------------------------------
-- NOTE: assuming that state isJust
agentState :: AgentOut s m e -> s
agentState = fromJust . aoState

-- NOTE: assuming that state isJust
updateAgentState :: (s -> s) -> AgentOut s m e -> AgentOut s m e
updateAgentState f ao = ao { aoState = Just $ f $ fromJust $ aoState ao }

setAgentState :: s -> AgentOut s m e -> AgentOut s m e
setAgentState s ao = updateAgentState (const s) ao

-------------------------------------------------------------------------------
-- Conversations
-------------------------------------------------------------------------------
hasConversation :: AgentOut s m e -> Bool
hasConversation = isEvent . aoConversationRequest

conversation :: AgentMessage m
                -> AgentConversationSender s m e
                -> AgentOut s m e
                -> AgentOut s m e
conversation msg replyHdl ao = ao { aoConversationRequest = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m e -> AgentOut s m e
conversationEnd ao = ao { aoConversationRequest = NoEvent }

-------------------------------------------------------------------------------
-- RECURSION
-------------------------------------------------------------------------------
agentRecursions :: AgentIn s m e -> Event [(AgentOut s m e, e)]
agentRecursions = aiRec

recInitAllowed :: AgentIn s m e -> Bool
recInitAllowed = aiRecInitAllowed

allowsRecOthers :: AgentOut s m e -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut s m e -> AgentOut s m e
recursive  allowOthers aout = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m e -> AgentOut s m e
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m e -> Bool
isRecursive ain = isEvent $ aiRec ain

-------------------------------------------------------------------------------
-- PURE WRAPPERS
-------------------------------------------------------------------------------
agentPure :: AgentPureBehaviour s m e -> AgentBehaviour s m e
agentPure f = proc (ain, e) -> do
  t <- time -< ()
  let (ao, e') = f e t ain
  returnA -< (ao, e')

agentPureReadEnv :: AgentPureBehaviourReadEnv s m e -> AgentBehaviour s m e
agentPureReadEnv f = proc (ain, e) -> do
  t <- time -< ()
  let ao = f e t ain
  returnA -< (ao, e)

agentPureIgnoreEnv :: AgentPureBehaviourNoEnv s m e -> AgentBehaviour s m e
agentPureIgnoreEnv f = proc (ain, e) -> do
  t <- time -< ()
  let ao = f t ain
  returnA -< (ao, e)

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
startingAgentIn :: [AgentDef s m e] -> TVar Int -> [AgentIn s m e]
startingAgentIn adefs idGen = map (startingAgentInFromAgentDef idGen) adefs

startingAgent :: [AgentDef s m e] -> TVar Int -> ([AgentBehaviour s m e], [AgentIn s m e])
startingAgent adefs idGen = (sfs, ains)
  where
    ains = startingAgentIn adefs idGen
    sfs = map adBeh adefs 

startingAgentInFromAgentDef :: TVar Int -> AgentDef s m e -> AgentIn s m e
startingAgentInFromAgentDef idGen ad = 
  AgentIn { aiId = adId ad
          , aiMessages = adInitMessages ad
          , aiConversationIncoming = NoEvent
          , aiStart = Event ()
          , aiRec = NoEvent
          , aiRecInitAllowed = True
          , aiIdGen = idGen 
          }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
agentOutAux :: Maybe s -> AgentOut s m e
agentOutAux s = 
  AgentOut {  aoKill = NoEvent
            , aoCreate = NoEvent
            , aoMessages = NoEvent
            , aoConversationRequest = NoEvent
            , aoConversationReply = NoEvent
            , aoState = s
            , aoRec = NoEvent
            , aoRecOthersAllowed = True
            } 