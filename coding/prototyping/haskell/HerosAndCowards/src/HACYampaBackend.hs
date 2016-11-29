{-# LANGUAGE Arrows #-}
module HACYampaBackend where

import Debug.Trace

import FRP.Yampa
import FRP.Yampa.Switches

import HACAgent as Agent
import HACSimulation as Sim

type ActiveAgent = SF AgentIn AgentOut

{-
NOTE: I think i have to split the whole process and procHelper thing:
the first call to process uses initialAgents as input without arror-rec!
then subsequent recursions are using the previously output-states as the input for the next recursion
-}
process :: [AgentState] -> SF Sim.SimIn Sim.SimOut
process initAgentStates = proc simIn ->
    do
        agentOuts <- (procHelper initAgentStates) -< simIn
        returnA -< Sim.SimOut{ simOutAllAgents = agentOuts }


procHelper :: [AgentState] -> SF (Sim.SimIn) [AgentOut]
procHelper agentStates = dpSwitch
                            (route agentStates)
                            (agentsToSF agentStates)
                            (arr collectOutput >>> notYet)              -- Signal function that observes the external input signal and the output signals from the collection in order to produce a switching event.
                            continuation

{- Routing function. Its purpose is to pair up each running signal function
in the collection maintained by par with the input it is going to see
at each point in time. All the routing function can do is specify how the input is distributed.

1st argument:   the input
2nd argument:   the collection of signal-functions

returns:        a list of tuples where:
                    1st item of the tuple is the input to the signal-function a
                    2nd item is the signal-function
-}
route :: [AgentState] -> (Sim.SimIn) -> [sf] -> [(AgentIn, sf)]
route as (simIn) agentSFs = map (\sf -> (aIn, sf)) agentSFs
    where
        aIn = agentInFromAgents as

-- creates the initial collection of signal functions.
agentsToSF :: [AgentState] -> [ActiveAgent]
agentsToSF as = map activeAgent as

{- Signal function that observes the external input signal and
the output signals from the collection in order to produce a switching event.

1st argument:   tuple where the first is the input to the process SF and the second is the output of the running SFs
return:         an event with arbitrary data
-}
collectOutput :: ((Sim.SimIn), [AgentOut]) -> (Event [AgentState])
collectOutput ((simIn), newAgentOuts) = Event (map agentOutState newAgentOuts)

{- The fourth argument is a function that is invoked when the switching event occurs,
yielding a new signal function to switch into based on the collection of signal functions
previously running and the value carried by the switching event. This allows the collection
 to be updated and then switched back in, typically by employing dpSwitch again.
-}
continuation :: [ActiveAgent] -> [AgentState] -> SF (Sim.SimIn) [AgentOut]
continuation agentSFs newAgentStates = procHelper newAgentStates

-- TODO: problem integral / derivative does not work as imagined because for every iteration a new SF is created which begins with its time at 0 :(
activeAgent :: AgentState -> ActiveAgent
activeAgent a = proc agentIn ->
    do
        stepWidth <- integral -< Agent.agentSpeedPerTimeUnit
        let out = Agent.agentStep agentIn stepWidth a
        returnA -< out