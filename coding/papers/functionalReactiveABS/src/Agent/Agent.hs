{-# LANGUAGE Arrows #-}

module Agent.Agent where

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
    aoCreate :: [AgentDef s m],
    aoMessages :: Event [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s
}

----------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION WITH ITS OWN LOOP
----------------------------------------------------------------------------------------------------------------------
processIO :: [AgentDef s m] -> ([AgentOut s m] -> IO (Bool, Double)) -> IO ()
processIO as outFunc = do
                            hdl <- reactInit
                                        (return ains)
                                        (iter outFunc)
                                        (process as)
                            Agent.Agent.iterate hdl (1.0, Nothing) -- TODO: need to get this dt correct instead of 0.5, should come from input/output
                            return ()
                        where
                            ains = createStartingAgentIn as

iterate :: ReactHandle a b -> (DTime, Maybe a) -> IO Bool
iterate hdl (dt, input) = do
                            cont <- react hdl (1.0, Nothing)  -- TODO: need to get this dt correct instead of 0.5, should come from input/output
                            if cont then
                                Agent.Agent.iterate hdl (dt, input)
                                    else
                                        return False

-- NOTE: don't care about a, we don't use it anyway
iter :: ([AgentOut s m] -> IO (Bool, Double)) -> ReactHandle a [AgentOut s m] -> Bool -> [AgentOut s m] -> IO Bool
iter outFunc hdl _ out = do
                            (cont, dt) <- outFunc out
                            return cont
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION WITHIN AN OUTER LOOP
----------------------------------------------------------------------------------------------------------------------
-- NOTE: don't care about a, we don't use it anyway
processIOInit :: [AgentDef s m] -> (ReactHandle [AgentIn s m] [AgentOut s m] -> Bool -> [AgentOut s m] -> IO Bool) -> IO (ReactHandle [AgentIn s m] [AgentOut s m])
processIOInit as iterFunc = reactInit
                                (return ains)
                                iterFunc
                                (process as)
    where
        ains = createStartingAgentIn as

----------------------------------------------------------------------------------------------------------------------
-- CALCULATING A FIXED NUMBER OF STEPS OF THE SIMULATION
----------------------------------------------------------------------------------------------------------------------
{- NOTE: to run Yampa in a pure-functional way use embed -}
processSteps :: [AgentDef s m] -> Double -> Int -> [[AgentOut s m]]
processSteps as dt steps = embed
                            (process as)
                            (ains, sts)
    where

        -- NOTE: again haskells laziness put to use: take steps items from the infinite list of sampling-times
        sts = take steps $ samplingTimes 0 dt
        ains = createStartingAgentIn as

-- NOTE: this creates an infinite list of sampling-times with starting time t and sampling-interval dt
samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- EXPORTS OF AGENTS
----------------------------------------------------------------------------------------------------------------------
agentOutFromIn :: AgentIn s m -> AgentOut s m
agentOutFromIn ai = AgentOut{ aoId = (aiId ai),
                              aoKill = NoEvent,
                              aoCreate = [],
                              aoMessages = NoEvent,
                              aoState = (aiState ai) }

sendMessage :: AgentOut s m -> AgentMessage m -> AgentOut s m
sendMessage ao msg = ao { aoMessages = mergedMsgs }
    where
        newMsgEvent = Event [msg]
        existingMsgEvent = aoMessages ao
        mergedMsgs = mergeMessages existingMsgEvent newMsgEvent

sendMessages :: AgentOut s m -> [AgentMessage m] -> AgentOut s m
sendMessages ao msgs = foldl (\ao' msg -> sendMessage ao' msg ) ao msgs

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
    | otherwise = foldl (\ao' msg -> evtHdl ao' msg ) ao filteredMsgs
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
createStartingAgentIn as = map startingAgentInFromAgent as
    where
        startingAgentInFromAgent :: AgentDef s m -> AgentIn s m
        startingAgentInFromAgent a = AgentIn { aiId = (adId a),
                                                aiMessages = NoEvent,
                                                aiStart = Event (),
                                                aiStop = NoEvent,
                                                aiTerminate = NoEvent,
                                                aiState = (adState a)}

process :: [AgentDef s m] -> SF [AgentIn s m] [AgentOut s m]
process as = process' asfs
    where
        asfs = map adBehaviour as

process' :: [AgentBehaviour s m] -> SF [AgentIn s m] [AgentOut s m]
process' asfs  = proc ains ->
    do
        aos <- dpSwitch
                   route
                   asfs
                   (arr collectOutput >>> notYet)  -- TODO: WHY??? in the first iteration we don't fire yet >>> notYet
                   feedBack -< ains
        returnA -< aos

route :: [AgentIn s m] -> [sf] -> [(AgentIn s m, sf)]
route ains sfs = zip ains sfs


collectOutput :: ([AgentIn s m], [AgentOut s m]) -> (Event ([AgentOut s m], [AgentIn s m]))
collectOutput (oldAgentIn, newAgentOuts) = Event (newAgentOuts, newAgentIns)
    where
        newAgentIns = map (agentOutToAgentIn newAgentOuts) (zip oldAgentIn newAgentOuts)

        -- TODO: optimize by using a Map (would it help?) so we need to go over the agents once?
        agentOutToAgentIn :: [AgentOut s m] -> (AgentIn s m, AgentOut s m) -> AgentIn s m
        agentOutToAgentIn allOuts (oldIn, newOut) = oldIn { aiStart = NoEvent,
                                                            aiState = (aoState newOut),
                                                            aiMessages = msgEvt }
            where
                msgEvt = collectMessagesFor (aiId oldIn) allOuts

        collectMessagesFor :: AgentId -> [AgentOut s m] -> Event [AgentMessage m]
        collectMessagesFor aid aos = foldl (\accMsgs ao -> mergeMessages (collectMessagesFrom aid ao) accMsgs ) NoEvent aos
            where -- NOTE: this consumes 96% of alloc and 27.2% of the CPU time (most of it)

                collectMessagesFrom :: AgentId -> AgentOut s m -> Event [AgentMessage m]
                collectMessagesFrom aid ao = foldl (\accMsgs (receiverId, m) -> if receiverId == aid then
                                                                                mergeMessages (Event [(senderId, m)]) accMsgs
                                                                                else
                                                                                    accMsgs) NoEvent msgs
                    where
                        senderId = aoId ao
                        msgsEvt = aoMessages ao
                        msgs = if isEvent msgsEvt then
                                    fromEvent msgsEvt
                                    else
                                        []

-- TODO: add/remove signal-functions on agent creation/destruction
feedBack :: [AgentBehaviour s m] -> ([AgentOut s m], [AgentIn s m]) -> SF [AgentIn s m] [AgentOut s m]
feedBack asfs (newAgentOuts, newAgentIns) = proc _ ->
                                                do
                                                    aos <- (process' asfs) -<  newAgentIns
                                                    returnA -< aos

mergeMessages :: Event [AgentMessage m] -> Event [AgentMessage m] -> Event [AgentMessage m]
mergeMessages l r = mergeBy (\msgsLeft msgsRight -> msgsLeft ++ msgsRight) l r