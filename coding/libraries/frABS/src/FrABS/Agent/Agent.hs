module FrABS.Agent.Agent where

import FrABS.Env.Environment

import FRP.Yampa

import System.Random
import Control.Monad.Random
import Control.Monad
import Data.List

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m ec l = SF (AgentIn s m ec l) (AgentOut s m ec l)
type MessageFilter m = (AgentMessage m -> Bool)

type AgentConversationReply s m ec l = Maybe (m, (AgentIn s m ec l))

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
    aiRec :: Event ([AgentOut s m ec l]),
    aiRecInitAllowed :: Bool,
    aiRng :: StdGen
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
-- NOTE: beware of a = AgentOut (randomly manipulating AgentOut) because one will end up with 2 versions of AgentOut which need to be merged
runAgentRandom :: AgentOut s m ec l -> Rand StdGen a -> (a, AgentOut s m ec l)
runAgentRandom a f = (ret, a')
    where
        g = aoRng a
        (ret, g') = runRand f g
        a' = a {aoRng = g'}

drawRandomRangeFromAgent :: (Random a) => AgentOut s m ec l -> (a, a) -> (a, AgentOut s m ec l)
drawRandomRangeFromAgent a r = runAgentRandom a (getRandomR r)

drawMultipleRandomRangeFromAgent :: (Random a) => AgentOut s m ec l -> (a, a) -> Int -> ([a], AgentOut s m ec l)
drawMultipleRandomRangeFromAgent a r n = runAgentRandom a blub
    where
        blub = do
                infRand <- getRandomRs r
                let nRand = take n infRand
                return nRand


splitRandomFromAgent :: AgentOut s m ec l -> (StdGen, AgentOut s m ec l)
splitRandomFromAgent a = runAgentRandom a getSplit

agentPickRandom :: AgentOut s m ec l -> [a] -> (a, AgentOut s m ec l)
agentPickRandom a xs
    | null xs = error "cannot draw random element from empty list"
    | otherwise = (randElem, a')
    where
        cellCount = length xs
        (randIdx, a') = drawRandomRangeFromAgent a (0, cellCount - 1)
        randElem = xs !! randIdx

agentPickRandomMultiple :: AgentOut s m ec l -> [a] -> Int -> ([a], AgentOut s m ec l)
agentPickRandomMultiple a xs n
    | null xs = error "cannot draw random element from empty list"
    | otherwise = (randElems, a')
    where
        cellCount = length xs
        (randIndices, a') = drawMultipleRandomRangeFromAgent a (0, cellCount - 1) n
        randElems = foldr (\idx acc -> (xs !! idx) : acc) [] randIndices  

recInitAllowed :: AgentIn s m ec l -> Bool
recInitAllowed = aiRecInitAllowed

agentOutFromIn :: AgentIn s m ec l -> AgentOut s m ec l
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
    
hasMessage :: (Eq m) => AgentIn s m ec l -> m -> Event ()
hasMessage ai m
    | not hasAnyMessage = NoEvent
    | otherwise = if hasMsg then Event () else NoEvent
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

updateState :: AgentOut s m ec l -> (s -> s) -> AgentOut s m ec l
updateState ao sfunc = ao { aoState = s' }
    where
        s = aoState ao
        s' = sfunc s

allowsRecOthers :: AgentOut s m ec l -> Bool
allowsRecOthers = aoRecOthersAllowed

recursive :: AgentOut s m ec l -> Bool -> AgentOut s m ec l
recursive aout allowOthers = aout { aoRec = Event (), aoRecOthersAllowed = allowOthers }

unrecursive :: AgentOut s m ec l -> AgentOut s m ec l
unrecursive aout = aout { aoRec = NoEvent }

isRecursive :: AgentIn s m ec l -> Bool
isRecursive ain = isEvent $ aiRec ain

createStartingAgentIn :: [AgentDef s m ec l] -> Environment ec l -> [AgentIn s m ec l]
createStartingAgentIn as env = map (startingAgentInFromAgentDef env) as

startingAgentInFromAgentDef :: Environment ec l -> AgentDef s m ec l -> AgentIn s m ec l
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