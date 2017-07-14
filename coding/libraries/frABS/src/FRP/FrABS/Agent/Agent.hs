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

import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Simulation.Internal

import FRP.Yampa

import Control.Concurrent.STM.TVar

import System.Random
import Data.List

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m ec l = SF (AgentIn s m ec l) (AgentOut s m ec l)
type MessageFilter m = (AgentMessage m -> Bool)

type AgentConversationReply s m ec l = Maybe (m, AgentIn s m ec l)

type AgentConversationReceiver s m ec l = (AgentIn s m ec l
                                            -> AgentMessage m
                                            -> AgentConversationReply s m ec l) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging

type AgentConversationSender s m ec l = (AgentOut s m ec l
                                        -> Maybe (AgentMessage m)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> AgentOut s m ec l)

data AgentDef s m ec l = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBeh :: AgentBehaviour s m ec l,
    adConversation :: Maybe (AgentConversationReceiver s m ec l),
    adInitMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    adEnvPos :: EnvCoord,
    adRng :: StdGen
}

data AgentIn s m ec l = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiConversation :: Maybe (AgentConversationReceiver s m ec l),
    aiStart :: Event (),
    aiState :: s,
    aiEnv :: Environment ec l,
    aiEnvPos :: EnvCoord,
    aiRec :: Event [AgentOut s m ec l],
    aiRecInitAllowed :: Bool,
    aiRng :: StdGen,

    aiIdGen :: TVar Int
}

-- TODO: remove aoId, should be static
data AgentOut s m ec l = AgentOut {
    aoId :: AgentId,    
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m ec l],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoConversation :: Event (AgentMessage m, AgentConversationSender s m ec l),
    aoState :: s,
    aoEnv :: Environment ec l,
    aoEnvPos :: EnvCoord,
    aoRec :: Event (),
    aoRecOthersAllowed :: Bool,
    aoRng :: StdGen
}

------------------------------------------------------------------------------------------------------------------------
-- Agent Functions
------------------------------------------------------------------------------------------------------------------------
recInitAllowed :: AgentIn s m ec l -> Bool
recInitAllowed = aiRecInitAllowed

agentOutFromIn :: AgentIn s m ec l -> AgentOut s m ec l
agentOutFromIn ai = AgentOut{ aoId = aiId ai,
                              aoKill = NoEvent,
                              aoCreate = NoEvent,
                              aoMessages = NoEvent,
                              aoConversation = NoEvent,
                              aoState = aiState ai,
                              aoEnv = aiEnv ai,
                              aoRec = NoEvent,
                              aoEnvPos = aiEnvPos ai,
                              aoRecOthersAllowed = True,
                              aoRng = aiRng ai }

hasConversation :: AgentOut s m ec l -> Bool
hasConversation = isEvent . aoConversation

conversation :: AgentMessage m
                -> AgentConversationSender s m ec l
                -> AgentOut s m ec l
                -> AgentOut s m ec l
conversation msg replyHdl ao = ao { aoConversation = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m ec l -> AgentOut s m ec l
conversationEnd ao = ao { aoConversation = NoEvent }

sendMessage :: AgentMessage m -> AgentOut s m ec l -> AgentOut s m ec l
sendMessage msg ao = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

sendMessages :: [AgentMessage m] -> AgentOut s m ec l ->  AgentOut s m ec l
sendMessages msgs ao = foldr (\msg ao' -> sendMessage msg ao') ao msgs

broadcastMessage :: m -> [AgentId] -> AgentOut s m ec l -> AgentOut s m ec l
broadcastMessage m receiverIds ao = sendMessages msgs ao
    where
        n = length receiverIds
        ms = replicate n m
        msgs = zip receiverIds ms

createAgent :: AgentDef s m ec l -> AgentOut s m ec l -> AgentOut s m ec l
createAgent newDef ao = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

nextAgentId :: AgentIn s m ec l -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut s m ec l -> AgentOut s m ec l
kill ao = ao { aoKill = Event () }

isDead :: AgentOut s m ec l -> Bool
isDead = isEvent . aoKill

onStart :: (AgentOut s m ec l -> AgentOut s m ec l) -> AgentIn s m ec l -> AgentOut s m ec l -> AgentOut s m ec l
onStart evtHdl ai ao = onEvent evtHdl startEvt ao
    where
        startEvt = aiStart ai

onEvent :: (AgentOut s m ec l -> AgentOut s m ec l) -> Event () -> AgentOut s m ec l -> AgentOut s m ec l
onEvent evtHdl evt ao = if isEvent evt then
                            evtHdl ao
                            else
                                ao
    
hasMessage :: (Eq m) => m -> AgentIn s m ec l -> Bool
hasMessage m ai
    | not hasAnyMessage = False
    | otherwise = hasMsg
    where
        msgsEvt = aiMessages ai
        hasAnyMessage = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        hasMsg = Data.List.any ((==m) . snd) msgs

onMessage :: (acc -> AgentMessage m -> acc) -> AgentIn s m ec l -> acc -> acc
onMessage  msgHdl ai a 
    | not hasMessages = a
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg ) a msgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt


onFilterMessage :: MessageFilter m -> (acc -> AgentMessage m -> acc) -> AgentIn s m ec l -> acc -> acc
onFilterMessage msgFilter msgHdl ai acc
    | not hasMessages = acc
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg ) acc filteredMsgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        filteredMsgs = filter msgFilter msgs

onMessageFrom :: AgentId -> (acc -> AgentMessage m -> acc) -> AgentIn s m ec l -> acc -> acc
onMessageFrom senderId msgHdl ai acc = onFilterMessage filterBySender msgHdl ai acc
    where
        filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> (acc -> AgentMessage m -> acc) -> AgentIn s m ec l -> acc -> acc
onMessageType m msgHdl ai acc = onFilterMessage filterByMsgType msgHdl ai acc
    where
        filterByMsgType = ((==m) . snd) --(\(_, m') -> m == m' )

updateDomainState :: (s -> s) -> AgentOut s m ec l ->  AgentOut s m ec l
updateDomainState f ao = ao { aoState = s' }
    where
        s = aoState ao
        s' = f s
    
setDomainState :: s -> AgentOut s m ec l -> AgentOut s m ec l
setDomainState s ao = updateDomainState (\_ -> s) ao

allowsRecOthers :: AgentOut s m ec l -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: Bool -> AgentOut s m ec l -> AgentOut s m ec l
recursive  allowOthers aout = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m ec l -> AgentOut s m ec l
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m ec l -> Bool
isRecursive ain = isEvent $ aiRec ain

createStartingAgentIn :: [AgentDef s m ec l] -> Environment ec l -> TVar Int -> [AgentIn s m ec l]
createStartingAgentIn as env idGen = map (startingAgentInFromAgentDef env idGen) as

startingAgentInFromAgentDef :: Environment ec l -> TVar Int -> AgentDef s m ec l -> AgentIn s m ec l
startingAgentInFromAgentDef env idGen ad = AgentIn { aiId = adId ad,
                                                        aiMessages = adInitMessages ad,
                                                        aiConversation = adConversation ad,
                                                        aiStart = Event (),
                                                        aiState = adState ad,
                                                        aiEnv = env,
                                                        aiEnvPos = adEnvPos ad,
                                                        aiRec = NoEvent,
                                                        aiRecInitAllowed = True,
                                                        aiRng = adRng ad,
                                                        aiIdGen = idGen }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r

agentPure :: (Double -> AgentIn s m ec l -> AgentOut s m ec l -> AgentOut s m ec l) -> AgentBehaviour s m ec l
agentPure f = proc ain ->
    do
        age <- time -< 0

        let ao = agentOutFromIn ain
        let ao' = f age ain ao
        
        returnA -< ao'