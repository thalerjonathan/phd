{-# LANGUAGE Arrows #-}
module FrABS.Agent.Agent where

import FrABS.Env.Environment
-- TODO: problem this forms a cycle
--import FrABS.Simulation.Simulation

import FRP.Yampa

import Data.Maybe

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m ec = SF (AgentIn s m ec) (AgentOut s m ec)
type MessageFilter m = (AgentMessage m -> Bool)

data AgentDef s m ec = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBehaviour :: AgentBehaviour s m ec
}

-- TODO: need behaviour here
data AgentIn s m ec = AgentIn {
    aiId :: AgentId,
    aiMessages :: Event [AgentMessage m],     -- AgentId identifies sender
    aiStart :: Event (),
    aiStop :: Event (),
    aiTerminate :: Event(),
    aiState :: s,
    aiEnv :: Environment ec,
    aiRec :: Maybe (AgentRecursion s m ec)
}


data AgentRecursion s m ec = AgentRecursion {
    arecDepth :: Int,
    arecOtherIns :: [AgentIn s m ec],
    arecOtherSfs :: [AgentBehaviour s m ec]
}

-- TODO: need behaviour here
data AgentOut s m ec = AgentOut {
    aoId :: AgentId,
    aoKill :: Event (),
    aoCreate :: Event [AgentDef s m ec],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s,
    aoEnv :: Environment ec
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
                              aoEnv = (aiEnv ai)}

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

-- TODO: put SFs into AgentIn (frozen sf before execution) and AgentOut (frozen sf after execution)
-- TODO: need to have a mechanism for marking agents calculating recursions e.g. only the calling, neighbours, all, ...
-- TODO: how much does the order of sequential execution matters here? e.g. where should be place the new AgentIn
-- NOTE: in each recursion:
    -- 1. temporary new AgentIn is created from the AgentOut
    -- 2. the temp AgentIn is added to the other agent ins
    -- 3. the whole thing is simulated for a given number of steps, resulting in a list of list of Agent-Outs where the last list is the last step
    -- 4. find the AgentOut of the Agent and add it to the list of AgentOuts
    -- 5. recurr until depth has been reached
    -- 6. return the list of AgentOuts: each entry is the output of the corresponding agent of the last step for each recursion-depth
recursive :: AgentIn s m ec
                -> AgentOut s m ec
                -> Int
                -> Int
                -> Double
                -> Bool
                -> [AgentOut s m ec]
recursive originalAin aout depth steps dt parStrategy
    | isJust originalAinRec = []
    | otherwise = []
    where
        originalAinRec = (aiRec originalAin)

        arec = AgentRecursion {
            arecDepth = 0,
            arecOtherIns = [], -- TODO: need the inputs
            arecOtherSfs = [] -- TODO: need the sfs
        }
        ainRec = AgentIn {  -- TODO take old ain
                     -- aiId :: AgentId,                            NOTE: taken from the old ain
                     -- aiMessages :: Event [AgentMessage m],       TODO: what should we do in this case?
                     -- aiStart :: Event (),
                     -- aiStop :: Event (),
                     -- aiTerminate :: Event(),
                     aiState = (aoState aout),
                     aiEnv = (aoEnv aout),
                     aiRec = Just arec
                 }

        recurrOneLevel :: Int
                          -> Int
                          -> Double
                          -> Bool
                          -> AgentRecursion s m ec
                          -> AgentIn s m ec
                          -> [[AgentOut s m ec]]
        recurrOneLevel depth steps dt parStrat arec ain
            | depth == currDepth = [] -- NOTE: reached recursion-depth, no more simulation
            | otherwise = [] -- aouts -- TODO: increment depth by 1
                where
                    currDepth = arecDepth arec
                    ains = arecOtherIns arec
                    asfs = arecOtherSfs arec
                    -- TODO: problem: circular includes
                    -- aouts = simulate ains asfs parStrat dt steps    -- NOTE: calculate outputs of this level
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
                                        aiEnv = env }

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r