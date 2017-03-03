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
    aiState :: s,
    aiBehaviour :: AgentBehaviour s m
}

data AgentOut s m = AgentOut {
    aoKill :: Event (),
    aoCreate :: [AgentDef s m],
    aoMessages :: [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s
}

{-
processIO :: [AgentDef s m] -> ([AgentOut s m] -> IO (Bool, Double)) -> IO ()
processIO as outFunc = do
                        hdl <- reactInit
                                    (return as)
                                    (iter outFunc)
                                    (process)
                                    Agent.Agent.iterate hdl (1.0, Nothing) -- TODO: need to get this dt correct instead of 0.5, should come from input/output
                        return ()

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
-}

{- NOTE: to run Yampa in a pure-functional way use embed -}
processSteps :: [AgentDef s m] -> Double -> Int -> [[AgentOut s m]]
processSteps as dt steps = embed
                            (process as)
                            ([], sts)
    where

        -- NOTE: again haskells laziness put to use: take steps items from the infinite list of sampling-times
        sts = take steps $ samplingTimes 0 dt

-- NOTE: this creates an infinite list of sampling-times with starting time t and sampling-interval dt
samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
process :: [AgentDef s m] -> SF [AgentIn s m] [AgentOut s m]
process initAds = proc _ ->
    do
        aos <- process' asfs ains -< []
        returnA -< aos

    where
        ains = createStartingAgentIn initAds
        asfs = map aiBehaviour ains

createStartingAgentIn :: [AgentDef s m] -> [AgentIn s m]
createStartingAgentIn as = map startingAgentInFromAgent as
    where
        startingAgentInFromAgent :: AgentDef s m -> AgentIn s m
        startingAgentInFromAgent a = AgentIn { aiId = (adId a),
                                                aiMessages = [],
                                                aiStart = Event (),
                                                aiStop = NoEvent,
                                                aiTerminate = NoEvent,
                                                aiState = (adState a),
                                                aiBehaviour = (adBehaviour a)}

process' :: [AgentBehaviour s m] -> [AgentIn s m] -> SF [AgentIn s m] [AgentOut s m]
process' asfs ains = proc _ ->
    do
        aos <- dpSwitch
                   route
                   asfs
                   (arr collectOutput >>> notYet)  -- NOTE: in the first iteration we don't fire yet
                   feedBack -< ains
        returnA -< aos


route :: [AgentIn s m] -> [sf] -> [(AgentIn s m, sf)]
route ains sfs = zip ains sfs

-- TODO: distribute messages here
collectOutput :: ([AgentIn s m], [AgentOut s m]) -> (Event ([AgentOut s m], [AgentIn s m]))
collectOutput (oldAgentIn, newAgentOuts) = Event (newAgentOuts, newAgentIns)
    where
        newAgentIns = map (agentOutToAgentIn) (zip oldAgentIn newAgentOuts)

        agentOutToAgentIn :: (AgentIn s m, AgentOut s m) -> AgentIn s m
        agentOutToAgentIn (oldIn, newOut) = oldIn { aiStart = NoEvent,
                                                    aiState = (aoState newOut) }

-- TODO: add/remove signal-functions on agent creation/destruction
feedBack :: [AgentBehaviour s m] -> ([AgentOut s m], [AgentIn s m]) -> SF [AgentIn s m] [AgentOut s m]
feedBack asfs (newAgentOuts, newAgentIns) = process' asfs newAgentIns

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
{-
{- NOTE: Here we are pairing up (zip) the inputs to each signal-function. Each agents SF receives the (new) agent-state
         itself and the (new) positions of the enemy and friend -}
routeStartingAgent :: [AgentDef s m] -> [sf] -> [(AgentIn s m, sf)]
routeStartingAgent as sfs = zip ains sfs
    where
        ains = createStartingAgentIn as

startAgents :: [AgentDef s m] -> SF [AgentIn s m] [AgentOut s m]
startAgents initAs = proc _ ->
    do
        aos <- par routeStartingAgent asfs -< initAs
        returnA -< aos
    where
        asfs = map adBehaviour initAs

processIO :: Sim.SimulationIO
processIO simIn outFunc = do
                    hdl <- reactInit
                                (return as)
                                (iter outFunc)
                                (process as wt)
                    HACYampaBackend.iterate hdl (0.5, Nothing) -- TODO: need to get this dt correct instead of 0.5, should come from input/output
                    return ()
                where
                    as = Sim.simInInitAgents simIn
                    wt = Sim.simInWorldType simIn

iterate :: ReactHandle a b -> (DTime, Maybe a) -> IO Bool
iterate hdl (dt, input) = do
    cont <- react hdl (0.5, Nothing)  -- TODO: need to get this dt correct instead of 0.5, should come from input/output
    if cont then
        HACYampaBackend.iterate hdl (dt, input)
            else
                return False

-- NOTE: don't care about a, we don't use it anyway
iter :: (Sim.SimOut -> IO (Bool, Double)) -> ReactHandle a Sim.SimOut -> Bool -> Sim.SimOut -> IO Bool
iter outFunc hdl _ out = do
                    (cont, dt) <- outFunc out
                    return cont

{- NOTE: to run Yampa in a pure-functional way use embed -}
processSteps :: Sim.SimulationStep
processSteps simIn dt steps = embed
                            (process as wt)
                            (as, sts)
    where
        as = Sim.simInInitAgents simIn
        wt = Sim.simInWorldType simIn
        -- NOTE: again haskells laziness put to use: take steps items from the infinite list of sampling-times
        sts = take steps $ samplingTimes 0 dt

-- NOTE: this creates an infinite list of sampling-times with starting time t and sampling-interval dt
samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt
-}

{-
process' :: [Agent] -> SF [AgentIn] [AgentOut]
process' asfs ain = proc _ ->
    do
        aos <- dpSwitch
                   route
                   asfs
                   (arr collectOutput >>> notYet)  -- NOTE: in the first iteration we don't fire yet
                   feedBack -< ain
        returnA -< aos

{- NOTE: Here we are pairing up (zip) the inputs to each signal-function. Each agents SF receives the (new) agent-state
         itself and the (new) positions of the enemy and friend -}
route :: [Agent] -> [sf] -> [(AgentIn, sf)]
route as sfs = zip ains sfs
    where
        ains = Agent.agentInFromAgents as

{- NOTE: Here we collect the output from the SFs and ALWAYS generate a switching event, so that we can create
         a new continuation with the new state but the old signal-functions. Keeping the old SFs is important as
         it means that we also keep the simulation time within each SF - if one creates a new SF, this means that
         in the new SF time starts from 0! -}
collectOutput :: ([Agent.AgentState], [Agent.AgentOut]) -> (Event [Agent.AgentState])
collectOutput (_, newAgentOuts) = Event (map Agent.agentOutState newAgentOuts)

{- NOTE: This is the function which creates the continuation after the event from collectOutput has fired (always).
         Because we need to feed-back the results into the next iteration we simply return process' with the existing
         signal-functions (=> not creating new ones, preserving time within each of them!) but with the new
         agent-states! -}
feedBack :: [AgentBehaviour] -> [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
feedBack asfs newAs = process' asfs newAs



{- NOTE: This is the signal-function of a running agent. Note that it does NOT receive an initial agent (like in the
         SpaceInvaders example of Yampa, and other Yampa-Game examples) as the current state is fed in through the
         input because it changes in each iteration: the output of the previous call to SF becomes the input for the
         next call to the SF. If we would pass an initial agent we would need to create SFs always new which would
         result in a restart of time thus making the proper use of time-related functions  like derivative/integral
         impossible / useless.
         The world-type is constant for all agents and also constant during the whole simulation, thus can be pased
         in as a "static" argument using currying. -}
activeAgent :: Agent -> AgentBehaviour
activeAgent initAgentDef = proc agentIn ->
    do
        t <- time -< ()
        -- NOTE: calculating the step-width based on the time which has passed since the last call. This can be achieved
        --       by using derivative which returns the difference between the current and the last value
        dt <- derivative -< t :: Double
        let stepWidth = 0.005 -- Agent.agentSpeedPerTimeUnit * dt -- TODO fix bug: goint too fast, actually it is not dt but 2*dt!
        let ao = Agent.agentStep wt stepWidth agentIn
        -- let ao = trace ("time=" ++ (show t) ++ ", derivative=" ++ (show d) ++ "stepwidth=" ++ (show stepWidth)) Agent.agentStep stepWidth agentIn
        returnA -< ao
-}