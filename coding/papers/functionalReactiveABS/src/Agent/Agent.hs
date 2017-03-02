{-# LANGUAGE Arrows #-}

module Agent.Agent where

import FRP.Yampa
import FRP.Yampa.Switches

type AgentId = Int
type AgentMessage m = (AgentId, m)
type AgentBehaviour s m = SF (AgentIn s m) (AgentOut s m)

data Agent s m = Agent {
    adId :: AgentId,
    adState :: s,
    adBehaviour :: AgentBehaviour s m
}

data AgentIn s m = AgentIn {
    aiMessages :: [AgentMessage m],     -- AgentId identifies sender
    aiStart :: Event (),
    aiStop :: Event (),
    aiTerminate :: Event(),
    aiState :: s
}

data AgentOut s m = AgentOut {
    aoKill :: Event (),
    aoCreate :: [Agent s m],
    aoMessages :: [AgentMessage m],     -- AgentId identifies receiver
    aoState :: s
}

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
{- NOTE: This is just the initial signal-function which will be use ONCE to kick-off the whole simulation:
         it just creates the signal-functions (one for each agent) and then passes them and the initial agent-states
         to the process'.
         SimOut is only used as an impostor for now, no real use but introduced for further usages when really necessary -}
process :: [Agent] -> SF [AgentIn] [AgentOut]
process initAs = proc _ ->
    do
        -- TODO: send start to all agents, this is done using par
        -- TODO: from AgentOut and Agent, create new AgentIn
        aos <- process' initASFs initAs -< []
        returnA -< aos
    where
        initASFs = map adBehaviour initAs

startAgents :: [Agent] -> SF [AgentIn] [AgentOut]
startAgents initAs = proc _ ->
    do
        let agentIns =
        aos <- par
                    route

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
