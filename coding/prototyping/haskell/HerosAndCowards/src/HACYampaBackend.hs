{-# LANGUAGE Arrows #-}
module HACYampaBackend where

import Debug.Trace

import FRP.Yampa
import FRP.Yampa.Switches

import qualified HACAgent as Agent

type ActiveAgent = SF Agent.AgentIn Agent.AgentOut

{- NOTE: problem is we have 3 versions of the agents at this point:
         1. initial agents in initAs
         2. input agents to SF in as
         3. output agents of SF in aos
    We cannot use rec in this case to feed the output back into the input because the output BECOMES the input and not
    a calculuation in it - this needs to be done outside
 -}
process :: [Agent.AgentState] -> SF [Agent.AgentState] [Agent.AgentOut]
process initAs = proc as ->
    do
        aos <- par route initialSFs -< as
        returnA -< aos
        where
            initialSFs = map activeAgent initAs

route :: [Agent.AgentState] -> [sf] -> [(Agent.AgentIn, sf)]
route as sfs = zip ains sfs
    where
        ains = Agent.agentInFromAgents as

activeAgent :: Agent.AgentState -> ActiveAgent
activeAgent a = proc agentIn ->
    do
        t <- time -< ()
        d <- derivative -< t :: Double
        stepWidth <- (* Agent.agentSpeedPerTimeUnit) ^<< derivative -< t
        --let ao = trace ("time=" ++ (show t) ++ ", derivative=" ++ (show d) ++ "stepwidth=" ++ (show stepWidth)) Agent.agentStep agentIn stepWidth a
        returnA -< Agent.agentStep stepWidth agentIn

