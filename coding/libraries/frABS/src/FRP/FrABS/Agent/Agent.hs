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

import FRP.FrABS.Env.Environment
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

sendMessage :: AgentOut s m ec l -> AgentMessage m -> AgentOut s m ec l
sendMessage ao msg = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

hasConversation :: AgentOut s m ec l -> Bool
hasConversation = isEvent . aoConversation

conversation :: AgentOut s m ec l
                -> AgentMessage m
                -> AgentConversationSender s m ec l
                -> AgentOut s m ec l
conversation ao msg replyHdl = ao { aoConversation = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m ec l -> AgentOut s m ec l
conversationEnd ao = ao { aoConversation = NoEvent }


sendMessages :: AgentOut s m ec l -> [AgentMessage m] -> AgentOut s m ec l
sendMessages ao msgs = foldr (\msg ao' -> sendMessage ao' msg ) ao msgs

broadcastMessage :: AgentOut s m ec l -> m -> [AgentId] -> AgentOut s m ec l
broadcastMessage ao m receiverIds = sendMessages ao msgs
    where
        n = length receiverIds
        ms = replicate n m
        msgs = zip receiverIds ms

createAgent :: AgentOut s m ec l -> AgentDef s m ec l -> AgentOut s m ec l
createAgent ao newDef = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

nextAgentId :: AgentIn s m ec l -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut s m ec l -> AgentOut s m ec l
kill ao = ao { aoKill = Event () }

isDead :: AgentOut s m ec l -> Bool
isDead = isEvent . aoKill

onStart :: AgentIn s m ec l -> (AgentOut s m ec l -> AgentOut s m ec l) -> AgentOut s m ec l -> AgentOut s m ec l
onStart ai evtHdl ao = onEvent startEvt evtHdl ao
    where
        startEvt = aiStart ai

onEvent :: Event () -> (AgentOut s m ec l -> AgentOut s m ec l) -> AgentOut s m ec l -> AgentOut s m ec l
onEvent evt evtHdl ao = if isEvent evt then
                            evtHdl ao
                            else
                                ao
    
hasMessage :: (Eq m) => AgentIn s m ec l -> m -> Bool
hasMessage ai m
    | not hasAnyMessage = False
    | otherwise = hasMsg
    where
        msgsEvt = aiMessages ai
        hasAnyMessage = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        hasMsg = Data.List.any ((==m) . snd) msgs

onMessage :: AgentIn s m ec l -> (acc -> AgentMessage m -> acc) -> acc -> acc
onMessage ai msgHdl a 
    | not hasMessages = a
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg ) a msgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt


onFilterMessage :: MessageFilter m -> AgentIn s m ec l -> (acc -> AgentMessage m -> acc) -> acc -> acc
onFilterMessage msgFilter ai msgHdl acc
    | not hasMessages = acc
    | otherwise = foldr (\msg acc'-> msgHdl acc' msg ) acc filteredMsgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        filteredMsgs = filter msgFilter msgs

onMessageFrom :: AgentId -> AgentIn s m ec l -> (acc -> AgentMessage m -> acc) -> acc -> acc
onMessageFrom senderId ai msgHdl acc = onFilterMessage filterBySender ai msgHdl acc
    where
        filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> AgentIn s m ec l -> (acc -> AgentMessage m -> acc) -> acc -> acc
onMessageType m ai msgHdl acc = onFilterMessage filterByMsgType ai msgHdl acc
    where
        filterByMsgType = ((==m) . snd) --(\(_, m') -> m == m' )

updateDomainState :: AgentOut s m ec l -> (s -> s) -> AgentOut s m ec l
updateDomainState ao sfunc = ao { aoState = s' }
    where
        s = aoState ao
        s' = sfunc s
    
setDomainState :: AgentOut s m ec l -> s -> AgentOut s m ec l
setDomainState ao s = updateDomainState ao (\_ -> s)

allowsRecOthers :: AgentOut s m ec l -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: AgentOut s m ec l -> Bool -> AgentOut s m ec l
recursive aout allowOthers = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

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