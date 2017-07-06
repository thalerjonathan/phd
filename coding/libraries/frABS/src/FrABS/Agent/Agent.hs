module FrABS.Agent.Agent (
    AgentId,
    AgentMessage,
    AgentBehaviour,

    AgentConversationReply,
    AgentConversationReceiver,
    AgentConversationSender,
    
    AgentDef (..),
    AgentIn (..),
    AgentOut (..),

    agentIdM,
    environmentM,
    environmentPositionM,
    changeEnvironmentPositionM,

    createAgent,
    createAgentM,
    kill,
    killM,
    isDead,
    isDeadM,

    createStartingAgentIn,
    agentOutFromIn,
    startingAgentInFromAgentDef,

    sendMessage,
    sendMessageM,
    sendMessages,
    sendMessagesM,
    broadcastMessage,
    broadcastMessageM,
    hasMessage,
    onMessage,
    onMessageM,
    onMessageMState,
    onFilterMessage,
    onMessageFrom,
    onMessageType,

    hasConversation,
    conversation,
    conversationM,
    conversationEnd,
    conversationEndM,
    conversationReplyMonadicRunner,
    conversationIgnoreReplyMonadicRunner,

    updateDomainState,
    updateDomainStateM,
    getDomainStateM,
    setDomainState,
    setDomainStateM,
    domainStateFieldM,

    runEnvironmentM,

    nextAgentId,

    onStart,
    onEvent,

    recInitAllowed,
    allowsRecOthers,
    recursive,
    unrecursive,
    isRecursive,

    mergeMessages
  ) where

import FrABS.Env.Environment
import FrABS.Simulation.Utils

import FRP.Yampa

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.State
import Control.Concurrent.STM.TVar

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
agentIdM :: State (AgentOut s m ec l) AgentId
agentIdM = state (\ao -> (aoId ao, ao))

environmentM :: State (AgentOut s m ec l) (Environment ec l)
environmentM = state (\ao -> (aoEnv ao, ao))

environmentPositionM :: State (AgentOut s m ec l) EnvCoord
environmentPositionM = state (\ao -> (aoEnvPos ao, ao))

changeEnvironmentPositionM :: EnvCoord -> State (AgentOut s m ec l) ()
changeEnvironmentPositionM pos = state (\ao -> ((), ao { aoEnvPos = pos }))

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

sendMessageM :: AgentMessage m -> State (AgentOut s m ec l) ()
sendMessageM msg = state (\ao -> ((), sendMessage ao msg))

hasConversation :: AgentOut s m ec l -> Bool
hasConversation = isEvent . aoConversation

conversation :: AgentOut s m ec l
                -> AgentMessage m
                -> AgentConversationSender s m ec l
                -> AgentOut s m ec l
conversation ao msg replyHdl = ao { aoConversation = Event (msg, replyHdl)}

conversationM :: AgentMessage m
                -> AgentConversationSender s m ec l
                -> State (AgentOut s m ec l) ()
conversationM msg replyHdl = state (\ao -> ((), conversation ao msg replyHdl))

conversationEnd :: AgentOut s m ec l -> AgentOut s m ec l
conversationEnd ao = ao { aoConversation = NoEvent }

conversationEndM :: State (AgentOut s m ec l) ()
conversationEndM = state (\ao -> ((), conversationEnd ao))

conversationReplyMonadicRunner :: (Maybe (AgentMessage m) -> State (AgentOut s m ec l) ()) 
                                    -> AgentConversationSender s m ec l
conversationReplyMonadicRunner replyAction ao mayReply = execState (replyAction mayReply) ao

conversationIgnoreReplyMonadicRunner :: State (AgentOut s m ec l) () -> AgentConversationSender s m ec l
conversationIgnoreReplyMonadicRunner replyAction ao _ = execState replyAction ao

sendMessages :: AgentOut s m ec l -> [AgentMessage m] -> AgentOut s m ec l
sendMessages ao msgs = foldr (\msg ao' -> sendMessage ao' msg ) ao msgs

sendMessagesM :: [AgentMessage m] -> State (AgentOut s m ec l) ()
sendMessagesM msgs = state (\ao -> ((), sendMessages ao msgs))

broadcastMessage :: AgentOut s m ec l -> m -> [AgentId] -> AgentOut s m ec l
broadcastMessage ao m receiverIds = sendMessages ao msgs
    where
        n = length receiverIds
        ms = replicate n m
        msgs = zip receiverIds ms

broadcastMessageM :: m -> [AgentId] -> State (AgentOut s m ec l) ()
broadcastMessageM m receiverIds = state (broadcastMessageMAux m)
    where
        broadcastMessageMAux :: m -> AgentOut s m ec l -> ((), AgentOut s m ec l)
        broadcastMessageMAux m ao = ((), sendMessages ao msgs)
            where
                n = length receiverIds
                ms = replicate n m
                msgs = zip receiverIds ms

createAgent :: AgentOut s m ec l -> AgentDef s m ec l -> AgentOut s m ec l
createAgent ao newDef = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

createAgentM :: AgentDef s m ec l -> State (AgentOut s m ec l) ()
createAgentM newDef = state (\ao -> ((),createAgent ao newDef))

nextAgentId :: AgentIn s m ec l -> AgentId
nextAgentId AgentIn { aiIdGen = idGen } = incrementAtomicallyUnsafe idGen

kill :: AgentOut s m ec l -> AgentOut s m ec l
kill ao = ao { aoKill = Event () }

killM :: State (AgentOut s m ec l) ()
killM = state (\ao -> ((), ao { aoKill = Event () }))

isDead :: AgentOut s m ec l -> Bool
isDead = isEvent . aoKill

isDeadM :: State (AgentOut s m ec l) Bool
isDeadM = state (\ao -> (isDead ao, ao))

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

onMessageMState :: AgentIn s m ec l -> (AgentMessage m -> State acc ()) -> State acc ()
onMessageMState ai msgHdl = onMessageM ai (\_ msg -> msgHdl msg) ()

onMessageM :: (Monad mon) => AgentIn s m ec l -> (acc -> AgentMessage m -> mon acc) -> acc -> mon acc
onMessageM ai msgHdl acc
    | not hasMessages = return acc
    -- | otherwise = foldM (\acc msg -> msgHdl acc msg) acc msgs
    | otherwise = foldM msgHdl acc msgs
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
    
updateDomainStateM :: (s -> s) -> State (AgentOut s m ec l) ()
updateDomainStateM sfunc = state (updateDomainStateMAux sfunc)
    where
        updateDomainStateMAux :: (s -> s) 
                            -> AgentOut s m ec l 
                            -> ((), AgentOut s m ec l)
        updateDomainStateMAux sfunc ao = ((), updateDomainState ao sfunc)

setDomainState :: AgentOut s m ec l -> s -> AgentOut s m ec l
setDomainState ao s = updateDomainState ao (\_ -> s)

setDomainStateM :: s -> State (AgentOut s m ec l) ()
setDomainStateM s = state (\ao -> ((), setDomainState ao s))

domainStateFieldM :: (s -> t) -> State (AgentOut s m ec l) t
domainStateFieldM f = state (domainStateFieldMAux f)
    where
        domainStateFieldMAux :: (s -> t) 
                            -> AgentOut s m ec l
                            -> (t, AgentOut s m ec l)
        domainStateFieldMAux f ao = (f s, ao)
            where
                s = aoState ao

runEnvironmentM :: State (Environment ec l) a -> State (AgentOut s m ec l) a
runEnvironmentM envStateTrans =
    do
        env <- environmentM 
        let (a, env') = runState envStateTrans env
        setEnvironmentM env'
        return a

setEnvironmentM :: Environment ec l -> State (AgentOut s m ec l) ()
setEnvironmentM env =
    do
        ao <- get 
        put $ ao { aoEnv = env }

getDomainStateM :: State (AgentOut s m ec l) s
getDomainStateM = 
    do
        ao <- get
        let domainState = aoState ao 
        return domainState

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