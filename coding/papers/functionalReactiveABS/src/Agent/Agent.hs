{-# LANGUAGE Arrows #-}

module Agent.Agent where

import FRP.Yampa
import FRP.Yampa.Switches

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m = SF (AgentIn s m) (AgentOut s m)

data AgentDef s m = AgentDef {
    adId :: AgentId,
    adState :: s,
    adBehaviour :: AgentBehaviour s m
}

data AgentIn s m = AgentIn {
    aiId :: AgentId,
    aiMessages :: [AgentMessage m],     -- AgentId identifies sender
    aiStart :: Event (),
    aiStop :: Event (),
    aiTerminate :: Event(),
    aiState :: s
}

data AgentOut s m = AgentOut {
    aoKill :: Event (),
    aoCreate :: [AgentDef s m],
    aoMessages :: [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s
}

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
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
createStartingAgentIn :: [AgentDef s m] -> [AgentIn s m]
createStartingAgentIn as = map startingAgentInFromAgent as
    where
        startingAgentInFromAgent :: AgentDef s m -> AgentIn s m
        startingAgentInFromAgent a = AgentIn { aiId = (adId a),
                                                aiMessages = [],
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

        agentOutToAgentIn :: [AgentOut s m] -> (AgentIn s m, AgentOut s m) -> AgentIn s m
        agentOutToAgentIn allOuts (oldIn, newOut) = oldIn { aiStart = NoEvent,
                                                            aiState = (aoState newOut),
                                                            aiMessages = msgs }
            where
                msgs = collectMessages (aiId oldIn) allOuts

        collectMessages :: AgentId -> [AgentOut s m] -> [AgentMessage m]
        collectMessages aid aos = foldl (\acc ao -> acc ++ collectMessages' aid ao ) [] aos
            where
                collectMessages' :: AgentId -> AgentOut s m -> [AgentMessage m]
                collectMessages' aid ao = foldl (\acc (receiverId, m) -> if receiverId == aid then
                                                                                acc ++ [(senderId, m)]
                                                                                else
                                                                                    acc ) [] (aoMessages ao)
                    where
                        senderId = aid -- TODO: use correct senderId, for now its the receiverId

-- TODO: add/remove signal-functions on agent creation/destruction
feedBack :: [AgentBehaviour s m] -> ([AgentOut s m], [AgentIn s m]) -> SF [AgentIn s m] [AgentOut s m]
feedBack asfs (newAgentOuts, newAgentIns) = proc _ ->
                                                do
                                                    aos <- (process' asfs) -<  newAgentIns
                                                    returnA -< aos