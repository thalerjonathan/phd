module FrABS.Agent.Agent where

import FrABS.Env.Environment

import FRP.Yampa

import System.Random
import Control.Monad.Random
import Control.Monad

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m ec = SF (AgentIn s m ec) (AgentOut s m ec)
type MessageFilter m = (AgentMessage m -> Bool)

type AgentConversationReply s m ec = Maybe (m, (AgentIn s m ec))

type AgentConversationReceiver s m ec = (AgentIn s m ec
                                            -> AgentMessage m
                                            -> AgentConversationReply s m ec) -- NOTE: the receiver MUST reply, otherwise we could've used the normal messaging

type AgentConversationSender s m ec = (AgentOut s m ec
                                        -> Maybe (AgentMessage m)   -- NOTE: this will be Nothing in case the conversation with the target was not established e.g. id not found, target got no receiving handler
                                        -> AgentOut s m ec)

data AgentDef s m ec = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBeh :: AgentBehaviour s m ec,
    adConversation :: Maybe (AgentConversationReceiver s m ec),
    adInitMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    adEnvPos :: EnvCoord,
    adRng :: StdGen
}

data AgentIn s m ec = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiConversation :: Maybe (AgentConversationReceiver s m ec),
    aiStart :: Event (),
    aiState :: s,
    aiEnv :: Environment ec,
    aiEnvPos :: EnvCoord,
    aiRec :: Event ([AgentOut s m ec]),
    aiRecInitAllowed :: Bool,
    aiRng :: StdGen
}

data AgentOut s m ec = AgentOut {
    aoId :: AgentId,
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m ec],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoConversation :: Event (AgentMessage m, AgentConversationSender s m ec),
    aoState :: s,
    aoEnv :: Environment ec,
    aoEnvPos :: EnvCoord,
    aoRec :: Event (),
    aoRecOthersAllowed :: Bool,
    aoRng :: StdGen
}

------------------------------------------------------------------------------------------------------------------------
-- Agent Functions
------------------------------------------------------------------------------------------------------------------------
-- NOTE: beware of a = AgentOut (randomly manipulating AgentOut) because one will end up with 2 versions of AgentOut which need to be merged
runAgentRandom :: AgentOut s m ec -> Rand StdGen a -> (a, AgentOut s m ec)
runAgentRandom a f = (ret, a')
    where
        g = aoRng a
        (ret, g') = runRand f g
        a' = a {aoRng = g'}

drawRandomRangeFromAgent :: (Random a) => AgentOut s m ec -> (a, a) -> (a, AgentOut s m ec)
drawRandomRangeFromAgent a r = runAgentRandom a (getRandomR r)

drawMultipleRandomRangeFromAgent :: (Random a) => AgentOut s m ec -> (a, a) -> Int -> ([a], AgentOut s m ec)
drawMultipleRandomRangeFromAgent a r n = runAgentRandom a blub
    where
        blub = do
                infRand <- getRandomRs r
                let nRand = take n infRand
                return nRand


splitRandomFromAgent :: AgentOut s m ec -> (StdGen, AgentOut s m ec)
splitRandomFromAgent a = runAgentRandom a getSplit

agentPickRandom :: AgentOut s m ec -> [a] -> (a, AgentOut s m ec)
agentPickRandom a xs
    | null xs = error "cannot draw random element from empty list"
    | otherwise = (randElem, a')
    where
        cellCount = length xs
        (randIdx, a') = drawRandomRangeFromAgent a (0, cellCount - 1)
        randElem = xs !! randIdx

agentPickRandomMultiple :: AgentOut s m ec -> [a] -> Int -> ([a], AgentOut s m ec)
agentPickRandomMultiple a xs n
    | null xs = error "cannot draw random element from empty list"
    | otherwise = (randElems, a')
    where
        cellCount = length xs
        (randIndices, a') = drawMultipleRandomRangeFromAgent a (0, cellCount - 1) n
        randElems = foldr (\idx acc -> (xs !! idx) : acc) [] randIndices  

recInitAllowed :: AgentIn s m ec -> Bool
recInitAllowed = aiRecInitAllowed

agentOutFromIn :: AgentIn s m ec -> AgentOut s m ec
agentOutFromIn ai = AgentOut{ aoId = (aiId ai),
                              aoKill = NoEvent,
                              aoCreate = NoEvent,
                              aoMessages = NoEvent,
                              aoConversation = NoEvent,
                              aoState = (aiState ai),
                              aoEnv = (aiEnv ai),
                              aoRec = NoEvent,
                              aoEnvPos = (aiEnvPos ai),
                              aoRecOthersAllowed = True,
                              aoRng = aiRng ai }

sendMessage :: AgentOut s m ec -> AgentMessage m -> AgentOut s m ec
sendMessage ao msg = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

hasConversation :: AgentOut s m ec -> Bool
hasConversation = isEvent . aoConversation

conversation :: AgentOut s m ec
                        -> AgentMessage m
                        -> AgentConversationSender s m ec
                        -> AgentOut s m ec
conversation ao msg replyHdl = ao { aoConversation = Event (msg, replyHdl)}

conversationEnd :: AgentOut s m ec -> AgentOut s m ec
conversationEnd ao = ao { aoConversation = NoEvent }

sendMessages :: AgentOut s m ec -> [AgentMessage m] -> AgentOut s m ec
sendMessages ao msgs = foldr (\msg ao' -> sendMessage ao' msg ) ao msgs

broadcastMessage :: AgentOut s m ec -> m -> [AgentId] -> AgentOut s m ec
broadcastMessage ao m receiverIds = sendMessages ao msgs
    where
        n = length receiverIds
        ms = replicate n m
        msgs = zip receiverIds ms

createAgent :: AgentOut s m ec -> AgentDef s m ec -> AgentOut s m ec
createAgent ao newDef = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

kill :: AgentOut s m ec -> AgentOut s m ec
kill ao = ao { aoKill = Event () }

isDead :: AgentOut s m ec -> Bool
isDead = isEvent . aoKill

onStart :: AgentIn s m ec -> (AgentOut s m ec -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onStart ai evtHdl ao = onEvent startEvt evtHdl ao
    where
        startEvt = aiStart ai

onEvent :: Event () -> (AgentOut s m ec -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onEvent evt evtHdl ao = if isEvent evt then
                            evtHdl ao
                            else
                                ao

onMessage :: MessageFilter m -> AgentIn s m ec -> (AgentOut s m ec -> AgentMessage m -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onMessage msgFilter ai evtHdl ao
    | not hasMessages = ao
    | otherwise = foldr (\msg ao'-> evtHdl ao' msg ) ao filteredMsgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        filteredMsgs = filter msgFilter msgs

onAnyMessage :: AgentIn s m ec -> (AgentOut s m ec -> AgentMessage m -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onAnyMessage ai evtHdl ao = onMessage noMsgFilter ai evtHdl ao
    where
        noMsgFilter = (\_ -> True)

onMessageFrom :: AgentId -> AgentIn s m ec -> (AgentOut s m ec -> AgentMessage m -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onMessageFrom senderId ai evtHdl ao = onMessage filterBySender ai evtHdl ao
    where
        filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> AgentIn s m ec -> (AgentOut s m ec -> AgentMessage m -> AgentOut s m ec) -> AgentOut s m ec -> AgentOut s m ec
onMessageType m ai evtHdl ao = onMessage filterByMsgType ai evtHdl ao
    where
        filterByMsgType = (\(_, m') -> m == m' )

updateState :: AgentOut s m ec -> (s -> s) -> AgentOut s m ec
updateState ao sfunc = ao { aoState = s' }
    where
        s = aoState ao
        s' = sfunc s

allowsRecOthers :: AgentOut s m ec -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: AgentOut s m ec -> Bool -> AgentOut s m ec
recursive aout allowOthers = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m ec -> AgentOut s m ec
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m ec -> Bool
isRecursive ain = isEvent $ aiRec ain

createStartingAgentIn :: [AgentDef s m ec] -> Environment ec -> [AgentIn s m ec]
createStartingAgentIn as env = map (startingAgentInFromAgentDef env) as

startingAgentInFromAgentDef :: Environment ec -> AgentDef s m ec -> AgentIn s m ec
startingAgentInFromAgentDef env ad = AgentIn { aiId = adId ad,
                                                aiMessages = adInitMessages ad,
                                                aiConversation = adConversation ad,
                                                aiStart = Event (),
                                                aiState = adState ad,
                                                aiEnv = env,
                                                aiEnvPos = adEnvPos ad,
                                                aiRec = NoEvent,
                                                aiRecInitAllowed = True,
                                                aiRng = adRng ad }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r