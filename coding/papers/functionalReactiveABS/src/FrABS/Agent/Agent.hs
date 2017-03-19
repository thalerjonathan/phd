{-# LANGUAGE Arrows #-}
module FrABS.Agent.Agent where

import FrABS.Env.Environment

import FRP.Yampa

import Data.Maybe

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m ec = SF (AgentIn s m ec) (AgentOut s m ec)
type MessageFilter m = (AgentMessage m -> Bool)

data AgentDef s m ec = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBeh :: AgentBehaviour s m ec
}

data AgentIn s m ec = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiStart :: Event (),
    aiStop :: Event (),
    aiTerminate :: Event(),
    aiState :: s,
    aiEnv :: Environment ec,
    aiRec :: Event ((Int, Int, Int), AgentOut s m ec)
}

data AgentOut s m ec = AgentOut {
    aoId :: AgentId,
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m ec],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s,
    aoEnv :: Environment ec,
    aoRec :: Event (Int, Int, Int)
}

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS OF AGENTS
----------------------------------------------------------------------------------------------------------------------
agentOutFromIn :: AgentIn s m ec -> AgentOut s m ec
agentOutFromIn ai = AgentOut{ aoId = (aiId ai),
                              aoKill = NoEvent,
                              aoCreate = NoEvent,
                              aoMessages = NoEvent,
                              aoState = (aiState ai),
                              aoEnv = (aiEnv ai),
                              aoRec = NoEvent}

sendMessage :: AgentOut s m ec -> AgentMessage m -> AgentOut s m ec
sendMessage ao msg = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

sendMessages :: AgentOut s m ec -> [AgentMessage m] -> AgentOut s m ec
sendMessages ao msgs = foldr (\msg ao' -> sendMessage ao' msg ) ao msgs

createAgent :: AgentOut s m ec -> AgentDef s m ec -> AgentOut s m ec
createAgent ao newDef = ao { aoCreate = createEvt }
    where
        oldCreateEvt = aoCreate ao
        createEvt = mergeBy (\leftCreate rightCreate -> leftCreate ++ rightCreate) (Event [newDef]) oldCreateEvt

kill :: AgentOut s m ec -> AgentOut s m ec
kill ao = ao { aoKill = Event () }

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

recursive :: AgentOut s m ec -> Int -> Int -> AgentOut s m ec
recursive aout initDepth initSteps
    | isEvent recEvt = aout { aoRec = Event (initDepth, initDepth, initSteps) }
    | otherwise = aout { aoRec = newRec }
    where
        recEvt = aoRec aout
        (totalDepth, currDepth, steps) = fromEvent recEvt
        newDepth = currDepth - 1
        newRec = if newDepth <= 0 then NoEvent else Event (totalDepth, currDepth - 1, steps)
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
createStartingAgentIn :: [AgentDef s m ec] -> Environment ec -> [AgentIn s m ec]
createStartingAgentIn as env = map (startingAgentInFromAgentDef env) as

startingAgentInFromAgentDef :: Environment ec -> AgentDef s m ec -> AgentIn s m ec
startingAgentInFromAgentDef env a = AgentIn { aiId = (adId a),
                                        aiMessages = NoEvent,
                                        aiStart = Event (),
                                        aiStop = NoEvent,
                                        aiTerminate = NoEvent,
                                        aiState = (adState a),
                                        aiEnv = env,
                                        aiRec = NoEvent }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r