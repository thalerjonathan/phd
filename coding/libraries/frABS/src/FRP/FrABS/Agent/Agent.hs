{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Agent (
    AgentId,
    AgentMessage,
    AgentBehaviour,

    AgentConversationReply,
    AgentConversationReceiver,
    AgentConversationSender,
    
    AgentDef (..),
    AgentIn (..),
    AgentOut (..),

    createAgent,
    kill,
    isDead,

    createStartingAgentIn,
    agentOutFromIn,
    startingAgentInFromAgentDef,

    sendMessage,
    sendMessages,
    broadcastMessage,
    hasMessage,
    onMessage,
    onFilterMessage,
    onMessageFrom,
    onMessageType,

    hasConversation,
    conversation,
    conversationEnd,

    updateDomainState,
    setDomainState,

    nextAgentId,

    onStart,
    onEvent,

    recInitAllowed,
    allowsRecOthers,
    recursive,
    unrecursive,
    isRecursive,

    mergeMessages,

    agentPure
  ) where

import FRP.FrABS.Simulation.Internal

import FRP.Yampa

import Control.Concurrent.STM.TVar

import System.Random
import Data.List

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m e = SF (AgentIn s m e, e) (AgentOut s m e, e)
type MessageFilter m = (AgentMessage m -> Bool)

type AgentConversationReply s m e = Maybe (m, (AgentIn s m e, e))

type AgentConversationReceiver s m e = ((AgentIn s m e, e)
                                            -> AgentMessage m
                                            -> AgentConversationReply s m e) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging

type AgentConversationSender s m e = ((AgentOut s m e, e)
                                        -> Maybe (AgentMessage m)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> (AgentOut s m e, e))

data AgentDef s m e = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBeh :: AgentBehaviour s m e,
    adConversation :: Maybe (AgentConversationReceiver s m e),
    adInitMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    adRng :: StdGen
}

data AgentIn s m e = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiConversation :: Maybe (AgentConversationReceiver s m e),
    aiStart :: Event (),
    aiState :: s,
    aiRec :: Event [(AgentOut s m e, e)],
    aiRecInitAllowed :: Bool,
    aiRng :: StdGen,
    aiIdGen :: TVar Int
}

data AgentOut s m e = AgentOut {
    aoId :: AgentId,    
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m e],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoConversation :: Event (AgentMessage m, AgentConversationSender s m e),
    aoState :: s,
    aoRec :: Event (),
    aoRecOthersAllowed :: Bool,
    aoRng :: StdGen
}

------------------------------------------------------------------------------------------------------------------------
-- Agent Functions
------------------------------------------------------------------------------------------------------------------------
recInitAllowed :: AgentIn s m e -> Bool
recInitAllowed = aiRecInitAllowed

agentOutFromIn :: AgentIn s m e -> AgentOut s m e
agentOutFromIn ai = AgentOut{   aoId = aiId ai,
                                aoKill = NoEvent,
                                aoCreate = NoEvent,
                                aoMessages = NoEvent,
                                aoConversation = NoEvent,
                                aoState = aiState ai,
                                aoRec = NoEvent,
                                aoRecOthersAllowed = True,
                                aoRng = aiRng ai }

hasConversation :: AgentOut s m e -> Bool
hasConversation = isEvent . aoConversation

conversation :: AgentMessage m
                -> AgentConversationSender s m e
                -> AgentOut s m e
                -> AgentOut s m e
conversation msg replyHdl ao = ao { aoConversation = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m e -> AgentOut s m e
conversationEnd ao = ao { aoConversation = NoEvent }

sendMessage :: AgentMessage m -> AgentOut s m e -> AgentOut s m e
sendMessage msg ao = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

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

onMessage :: (acc -> AgentMessage m -> acc) -> AgentIn s m e -> acc -> acc
onMessage  msgHdl ai a 
    | not hasMessages = a
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg) a msgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt


onFilterMessage :: MessageFilter m -> (acc -> AgentMessage m -> acc) -> AgentIn s m e -> acc -> acc
onFilterMessage msgFilter msgHdl ai acc
    | not hasMessages = acc
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg) acc filteredMsgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        filteredMsgs = filter msgFilter msgs

onMessageFrom :: AgentId -> (acc -> AgentMessage m -> acc) -> AgentIn s m e -> acc -> acc
onMessageFrom senderId msgHdl ai acc = onFilterMessage filterBySender msgHdl ai acc
    where
        filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> (acc -> AgentMessage m -> acc) -> AgentIn s m e -> acc -> acc
onMessageType m msgHdl ai acc = onFilterMessage filterByMsgType msgHdl ai acc
    where
        filterByMsgType = (==m) . snd --(\(_, m') -> m == m' )

updateDomainState :: (s -> s) -> AgentOut s m e ->  AgentOut s m e
updateDomainState f ao = ao { aoState = s' }
    where
        s = aoState ao
        s' = f s
    
setDomainState :: s -> AgentOut s m e -> AgentOut s m e
setDomainState s ao = updateDomainState (\_ -> s) ao

allowsRecOthers :: AgentOut s m e -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut s m e -> AgentOut s m e
recursive  allowOthers aout = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m e -> AgentOut s m e
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m e -> Bool
isRecursive ain = isEvent $ aiRec ain

createStartingAgentIn :: [AgentDef s m e] -> TVar Int -> [AgentIn s m e]
createStartingAgentIn as idGen = map (startingAgentInFromAgentDef idGen) as

startingAgentInFromAgentDef :: TVar Int -> AgentDef s m e -> AgentIn s m e
startingAgentInFromAgentDef idGen ad = AgentIn { aiId = adId ad,
                                                    aiMessages = adInitMessages ad,
                                                    aiConversation = adConversation ad,
                                                    aiStart = Event (),
                                                    aiState = adState ad,
                                                    aiRec = NoEvent,
                                                    aiRecInitAllowed = True,
                                                    aiRng = adRng ad,
                                                    aiIdGen = idGen }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r

agentPure :: (e -> Double -> AgentIn s m e -> AgentOut s m e -> (AgentOut s m e, e)) -> AgentBehaviour s m e
agentPure f = proc (ain, e) ->
    do
        age <- time -< 0

        let ao = agentOutFromIn ain
        let (ao', e') = f e age ain ao
        
        returnA -< (ao', e')