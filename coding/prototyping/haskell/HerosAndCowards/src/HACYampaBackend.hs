{-# LANGUAGE Arrows #-}
module HACYampaBackend (
    processIO,
    processSteps
  ) where

import Debug.Trace

import FRP.Yampa
import FRP.Yampa.Switches

import qualified HACAgent as Agent
import qualified HACSimulation as Sim

type ActiveAgent = SF Agent.AgentIn Agent.AgentOut

{-
main :: IO ()
main = do
    let g = mkStdGen rngSeed -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g agentCount heroDistribution
    reactimate (Main.init as) input output (YampaBack.process as)
    Front.shutdown
-- TODO: can remove passing initAs as they are ignored anyway
init :: [Agent.AgentState] -> IO [Agent.AgentState]
init initAs = do
    return initAs

input :: Double -> Bool -> IO (DTime, Maybe [Agent.AgentState])
input dt _ = do
    return (dt, Nothing)

output :: Bool -> [Agent.AgentOut] -> IO Bool
output _ aos = do
    winOpened <- Front.renderFrame aos
    return $ not winOpened
-}

{-
processIO :: [Agent.AgentState] -> (Sim.SimOut -> IO (Bool, Double)) -> IO Sim.SimOut
processIO as outFunc = do -- reactimate (init as) (input  output (YampaBack.process as)
            handle <- reactInit
                        (return as)
                        (iter outFunc)
                        (process as)
            cont <- react handle (0.0, Nothing) -- TODO: need to get this dt correct
            return Sim.SimOut { Sim.simOutAllAgents = [] }

iter :: (Sim.SimOut -> IO (Bool, Double)) -> ReactHandle a b -> Bool -> b -> IO Bool
iter outFunc hdl _ out = do
                    (cont, dt) <- outFunc out
                    return cont
-}

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
processIO :: Sim.SimulationIO
processIO simIn outFunc = do -- reactimate (init as) (input  output (YampaBack.process as)
                    hdl <- reactInit
                                (return as)
                                (iter outFunc)
                                (process as wt)
                    HACYampaBackend.iterate hdl (0.5, Nothing) -- TODO: need to get this dt correct
                    return ()
                where
                    as = Sim.simInInitAgents simIn
                    wt = Sim.simInWorldType simIn

iterate :: ReactHandle a b -> (DTime, Maybe a) -> IO Bool
iterate hdl (dt, input) = do
    cont <- react hdl (0.5, Nothing)  -- TODO: need to get this dt correct
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
process :: [Agent.AgentState] -> Agent.WorldType -> SF [Agent.AgentState] Sim.SimOut
process initAs wt = proc _ ->
    do
        aos <- process' initASFs initAs -< []
        returnA -< Sim.SimOut{ Sim.simOutAgents = aos }
    where
        initASFs = replicate (length initAs) (activeAgent wt)

{- NOTE: This is the real process-function where the real work happens. The point is that this simulation requires
         to feed-back the output back as input which can be done in multiple ways (see other implementations) but
         in this solution the focus was on: NOT RESTARTING AGENTS SIGNALFUNCTIONS and NO USAGE OF IORef.
         Passing in the already existing (old) signal-functions AND the (updated, new) agent-states from the previous
         iteration in this function the feed-back becomes possible utilizing a dpSwitch: collectOutput will collect
         the outputs of all the agents signal-functions and then fire an event which will be processed by feedBack to
         switch into a new signal-function which is just again this process' function but with the new agent-states but
         the old signal-functions
         TODO: why using (d)pSwitch and not just pSwitch? Do we really need the delay in this switch?
         Note that the input of the SF is not used in this function but the type is required for the route-function
         because the route-function first argument is the input of the SF: [Agent.AgentState] which is required to
         distribute the inputs to the signal-functions. -}
process' :: [ActiveAgent] -> [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
process' asfs as = proc _ ->
    do
        aos <- dpSwitch
                   route
                   asfs
                   (arr collectOutput >>> notYet)  -- NOTE: in the first iteration we don't fire yet
                   feedBack -< as
        returnA -< aos

{- NOTE: Here we are pairing up (zip) the inputs to each signal-function. Each agents SF receives the (new) agent-state
         itself and the (new) positions of the enemy and friend -}
route :: [Agent.AgentState] -> [sf] -> [(Agent.AgentIn, sf)]
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
feedBack :: [ActiveAgent] -> [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
feedBack asfs newAs = process' asfs newAs

{- NOTE: This is the signal-function of a running agent. Note that it does NOT receive an initial agent (like in the
         SpaceInvaders example of Yampa, and other Yampa-Game examples) as the current state is fed in through the
         input because it changes in each iteration: the output of the previous call to SF becomes the input for the
         next call to the SF. If we would pass an initial agent we would need to create SFs always new which would
         result in a restart of time thus making the proper use of time-related functions  like derivative/integral
         impossible / useless.
         The world-type is constant for all agents and also constant during the whole simulation, thus can be pased
         in as a "static" argument using currying. -}
activeAgent :: Agent.WorldType -> ActiveAgent
activeAgent wt = proc agentIn ->
    do
        t <- time -< ()
        -- NOTE: calculating the step-width based on the time which has passed since the last call. This can be achieved
        --       by using derivative which returns the difference between the current and the last value
        dt <- derivative -< t :: Double -- TODO fix bug: goint too fast, actually it is not dt but 2*dt!
        let stepWidth = Agent.agentSpeedPerTimeUnit * dt
        let ao = Agent.agentStep wt stepWidth agentIn
        -- let ao = trace ("time=" ++ (show t) ++ ", derivative=" ++ (show d) ++ "stepwidth=" ++ (show stepWidth)) Agent.agentStep stepWidth agentIn
        returnA -< ao
