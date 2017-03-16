{-# LANGUAGE Arrows #-}

module FrABS.Agent.Agent where

import FRP.Yampa
import FRP.Yampa.Switches

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m = SF (AgentIn s m) (AgentOut s m)
type MessageFilter m = (AgentMessage m -> Bool)

data AgentDef s m = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBehaviour :: AgentBehaviour s m
}

data AgentIn s m = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiStart :: Event (),
    aiStop :: Event (),
    aiTerminate :: Event(),
    aiState :: s
}

data AgentOut s m = AgentOut {
    aoId :: AgentId,
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s
}

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS OF AGENTS
----------------------------------------------------------------------------------------------------------------------
agentOutFromIn :: AgentIn s m -> AgentOut s m
agentOutFromIn ai = AgentOut{ aoId = (aiId ai),
                              aoKill = NoEvent,
                              aoCreate = NoEvent,
                              aoMessages = NoEvent,
                              aoState = (aiState ai) }

sendMessage :: AgentOut s m -> AgentMessage m -> AgentOut s m
sendMessage ao msg = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

sendMessages :: AgentOut s m -> [AgentMessage m] -> AgentOut s m
sendMessages ao msgs = foldr (\msg ao' -> sendMessage ao' msg ) ao msgs

createAgent :: AgentOut s m -> AgentDef s m -> AgentOut s m
createAgent ao newDef = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

kill :: AgentOut s m -> AgentOut s m
kill ao = ao { aoKill = Event () }

onStart :: AgentIn s m -> (AgentOut s m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onStart ai evtHdl ao = onEvent startEvt evtHdl ao
    where
        startEvt = aiStart ai

onEvent :: Event () -> (AgentOut s m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onEvent evt evtHdl ao = if isEvent evt then
                            evtHdl ao
                            else
                                ao

onMessage :: MessageFilter m -> AgentIn s m -> (AgentOut s m -> AgentMessage m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onMessage msgFilter ai evtHdl ao
    | not hasMessages = ao
    | otherwise = foldr (\msg ao'-> evtHdl ao' msg ) ao filteredMsgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
        filteredMsgs = filter msgFilter msgs

onAnyMessage :: AgentIn s m -> (AgentOut s m -> AgentMessage m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onAnyMessage ai evtHdl ao = onMessage noMsgFilter ai evtHdl ao
    where
        noMsgFilter = (\_ -> True)

onMessageFrom :: AgentId -> AgentIn s m -> (AgentOut s m -> AgentMessage m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onMessageFrom senderId ai evtHdl ao = onMessage filterBySender ai evtHdl ao
    where
        filterBySender = (\(senderId', _) -> senderId == senderId' )

onMessageType :: (Eq m) => m -> AgentIn s m -> (AgentOut s m -> AgentMessage m -> AgentOut s m) -> AgentOut s m -> AgentOut s m
onMessageType m ai evtHdl ao = onMessage filterByMsgType ai evtHdl ao
    where
        filterByMsgType = (\(_, m') -> m == m' )

updateState :: AgentOut s m -> (s -> s) -> AgentOut s m
updateState ao sfunc = ao { aoState = s' }
    where
        s = aoState ao
        s' = sfunc s
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
createStartingAgentIn :: [AgentDef s m] -> [AgentIn s m]
createStartingAgentIn as = map startingAgentInFromAgentDef as

startingAgentInFromAgentDef :: AgentDef s m -> AgentIn s m
startingAgentInFromAgentDef a = AgentIn { aiId = (adId a),
                                        aiMessages = NoEvent,
                                        aiStart = Event (),
                                        aiStop = NoEvent,
                                        aiTerminate = NoEvent,
                                        aiState = (adState a)}

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r