{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Agent 
  (
    AgentId
  , AgentMessage
  , AgentBehaviour

  -- AgentConversationReceiver
  , AgentConversationSender

  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)

  , AgentObservable

  , AgentPureBehaviour
  , AgentPureBehaviourReadEnv
  , AgentPureBehaviourNoEnv

  , agentId
  -- agentIdOut
  , createAgent
  , kill
  , isDead

  , createStartingAgentIn
  , createStartingAgent
  , agentOut
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

  -- agentStateIn
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

  , agentOutToObservable
  ) where

import Control.Concurrent.STM.TVar
import Data.List
import System.Random

import FRP.Yampa

import FRP.FrABS.Simulation.Internal

type AgentId                = Int
type AgentMessage m         = (AgentId, m)
type AgentBehaviour s m e   = SF (AgentIn s m e, e) (AgentOut s m e, e)
type MessageFilter m        = (AgentMessage m -> Bool)
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
  -- adState        :: !s
  , adBeh             :: AgentBehaviour s m e
  --adConversation  :: !(Maybe (AgentConversationReceiver s m e))
  , adInitMessages    :: !(Event [AgentMessage m])     -- AgentId identifies sender
  , adRng             :: !StdGen
  }

data AgentIn s m e = AgentIn 
  { aiId                    :: !AgentId
  , aiMessages              :: !(Event [AgentMessage m])     -- AgentId identifies sender
  -- aiConversation       :: !(Maybe (AgentConversationReceiver s m e)),
  , aiConversationIncoming  :: !(Event (AgentMessage m))
  , aiStart                 :: !(Event ())
  -- aiState              :: !s
  , aiRec                   :: !(Event [(AgentOut s m e, e)])
  , aiRecInitAllowed        :: !Bool
  , aiRng                   :: !StdGen
  , aiIdGen                 :: !(TVar Int)
  }

data AgentOut s m e = AgentOut 
  { -- aoId :: !AgentId   
    aoKill                  :: !(Event ())
  , aoCreate                :: !(Event [AgentDef s m e])
  , aoMessages              :: !(Event [AgentMessage m])     -- AgentId identifies receiver
  --aoConversation        :: !(Event (AgentMessage m, AgentConversationSender s m e))
  , aoConversationRequest   :: !(Event (AgentMessage m, AgentConversationSender s m e))
  , aoConversationReply     :: !(Event (AgentMessage m))
  , aoState                 :: !s -- observable state
  , aoRec                   :: !(Event ())
  , aoRecOthersAllowed      :: !Bool
  , aoRng                   :: !StdGen
  }

------------------------------------------------------------------------------------------------------------------------
-- Agent Functions
------------------------------------------------------------------------------------------------------------------------
agentId :: AgentIn s m e -> AgentId
agentId = aiId 

-- agentIdOut :: AgentOut s m e -> AgentId
-- agentIdOut = aoId 

agentRecursions :: AgentIn s m e -> Event [(AgentOut s m e, e)]
agentRecursions = aiRec

recInitAllowed :: AgentIn s m e -> Bool
recInitAllowed = aiRecInitAllowed

-- TODO: how can we get rid of agentin? the rng is the problem as it is the only reference to ain
agentOut :: s -> AgentIn s m e -> AgentOut s m e
agentOut s ai = 
  AgentOut {  aoKill = NoEvent
            , aoCreate = NoEvent
            , aoMessages = NoEvent
            , aoConversationRequest = NoEvent
            , aoConversationReply = NoEvent
            , aoState = s
            , aoRec = NoEvent
            , aoRecOthersAllowed = True
            , aoRng = aiRng ai 
            }  

agentOutToObservable :: AgentId -> AgentOut s m e -> AgentObservable s
agentOutToObservable aid ao = (aid, aoState ao)
 
hasConversation :: AgentOut s m e -> Bool
hasConversation = isEvent . aoConversationRequest

conversation :: AgentMessage m
                -> AgentConversationSender s m e
                -> AgentOut s m e
                -> AgentOut s m e
conversation msg replyHdl ao = ao { aoConversationRequest = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m e -> AgentOut s m e
conversationEnd ao = ao { aoConversationRequest = NoEvent }

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

createAgent :: AgentDef s m e -> AgentOut s m e -> AgentOut s m e
createAgent newDef ao = ao { aoCreate = createEvt }
  where
    oldCreateEvt = aoCreate ao
    createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

nextAgentId :: AgentIn s m e -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut s m e -> AgentOut s m e
kill ao = ao { aoKill = Event () }

isDead :: AgentOut s m e -> Bool
isDead = isEvent . aoKill

onStart :: (AgentOut s m e -> AgentOut s m e) -> AgentIn s m e -> AgentOut s m e -> AgentOut s m e
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
  where
    startEvt = aiStart ai

onEvent :: (AgentOut s m e -> AgentOut s m e) -> Event () -> AgentOut s m e -> AgentOut s m e
onEvent evtHdl evt ao = event ao (\_ -> evtHdl ao) evt
    
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
        filterByMsgType = (==m) . snd --(\(_, m') -> m == m' )

-- agentStateIn :: AgentIn s m e -> s
-- agentStateIn = aiState

agentState :: AgentOut s m e -> s
agentState = aoState

updateAgentState :: (s -> s) -> AgentOut s m e -> AgentOut s m e
updateAgentState f ao = ao { aoState = f $ aoState ao }

setAgentState :: s -> AgentOut s m e -> AgentOut s m e
setAgentState s ao = updateAgentState (const s) ao

allowsRecOthers :: AgentOut s m e -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut s m e -> AgentOut s m e
recursive  allowOthers aout = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m e -> AgentOut s m e
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m e -> Bool
isRecursive ain = isEvent $ aiRec ain

createStartingAgentIn :: [AgentDef s m e] -> TVar Int -> [AgentIn s m e]
createStartingAgentIn adefs idGen = map (startingAgentInFromAgentDef idGen) adefs

createStartingAgent :: [AgentDef s m e] -> TVar Int -> ([AgentBehaviour s m e], [AgentIn s m e])
createStartingAgent adefs idGen = (sfs, ains)
  where
    ains = createStartingAgentIn adefs idGen
    sfs = map adBeh adefs 

startingAgentInFromAgentDef :: TVar Int -> AgentDef s m e -> AgentIn s m e
startingAgentInFromAgentDef idGen ad = 
  AgentIn { aiId = adId ad
          , aiMessages = adInitMessages ad
          -- aiConversation = adConversation ad
          , aiConversationIncoming = NoEvent
          , aiStart = Event ()
          -- aiState = adState ad
          , aiRec = NoEvent
          , aiRecInitAllowed = True
          , aiRng = adRng ad
          , aiIdGen = idGen 
          }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r

agentPure :: AgentPureBehaviour s m e -> AgentBehaviour s m e
agentPure f = proc (ain, e) -> do
  t <- time -< ()
  --let ao = agentOutFromIn ain
  let (ao, e') = f e t ain
  returnA -< (ao, e')

agentPureReadEnv :: AgentPureBehaviourReadEnv s m e -> AgentBehaviour s m e
agentPureReadEnv f = proc (ain, e) -> do
  t <- time -< ()
  --let ao = agentOutFromIn ain
  let ao = f e t ain
  returnA -< (ao, e)

agentPureIgnoreEnv :: AgentPureBehaviourNoEnv s m e -> AgentBehaviour s m e
agentPureIgnoreEnv f = proc (ain, e) -> do
  t <- time -< ()
  --let ao = agentOutFromIn ain
  let ao = f t ain
  returnA -< (ao, e)