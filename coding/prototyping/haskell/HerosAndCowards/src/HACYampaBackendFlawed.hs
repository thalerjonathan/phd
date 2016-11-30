{-# LANGUAGE Arrows #-}
module HACYampaBackendFlawed where

import Debug.Trace

import FRP.Yampa
import FRP.Yampa.Switches

import qualified HACAgent as Agent

type ActiveAgent = SF Agent.AgentIn Agent.AgentOut

process :: [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
process initAs = proc as ->
    do
        aos <- (procHelper initAs) -< as
        returnA -< aos

procHelper :: [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
procHelper as = dpSwitch
                    (route as)
                    (agentsToSF as)
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
route :: [Agent.AgentState] -> [Agent.AgentState] -> [sf] -> [(Agent.AgentIn, sf)]
route as as' sfs = map (\sf -> (aIn, sf)) sfs
    where
        aIn = Agent.agentInFromAgents as

-- creates the initial collection of signal functions.
agentsToSF :: [Agent.AgentState] -> [ActiveAgent]
agentsToSF as = map activeAgent as

{- Signal function that observes the external input signal and
the output signals from the collection in order to produce a switching event.

1st argument:   tuple where the first is the input to the process SF and the second is the output of the running SFs
return:         an event with arbitrary data
-}
collectOutput :: ([Agent.AgentState], [Agent.AgentOut]) -> (Event [Agent.AgentState])
collectOutput (as, newAgentOuts) = Event (map Agent.agentOutState newAgentOuts)


{- The fourth argument is a function that is invoked when the switching event occurs,
yielding a new signal function to switch into based on the collection of signal functions
previously running and the value carried by the switching event. This allows the collection
 to be updated and then switched back in, typically by employing dpSwitch again.
-}
continuation :: [ActiveAgent] -> [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
continuation asfs newAs = procHelper newAs

activeAgent :: Agent.AgentState -> ActiveAgent
activeAgent a = proc agentIn -> do
    -- NOTE: problem integral / derivative does not work as imagined because for every iteration a new SF is created which begins with its time at 0 :(
    --stepWidth <- derivative -< Agent.agentSpeedPerTimeUnit
    let stepWidth = Agent.agentSpeedPerTimeUnit
    let ao = Agent.agentStep agentIn stepWidth a
    returnA -< ao

{- NOTE: this approach also doesn't work (besides that time restarts at 0) because when using (d)switch to switch
         into the same SF over again it will cause endless recursion -}
{-
activeAgent :: Agent.AgentState -> ActiveAgent
activeAgent a = dSwitch sf (\a -> activeAgent a )
    where
        sf :: SF Agent.AgentIn (Agent.AgentOut, Event Agent.AgentState)
        sf = proc agentIn ->
            do
                t <- time -< ()
                let ao = Agent.agentStep agentIn Agent.agentSpeedPerTimeUnit a
                let a' = Agent.agentOutState ao
                evt <- edge -< t > 0 -- NoEvent -- Event a'
                returnA -< (ao, evt `tag` a')
-}