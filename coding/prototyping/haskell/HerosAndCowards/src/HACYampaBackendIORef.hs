{-# LANGUAGE Arrows #-}
module HACYampaBackendIORef where

import Debug.Trace

import FRP.Yampa
import FRP.Yampa.Switches

import qualified HACAgent as Agent

type ActiveAgent = SF Agent.AgentIn Agent.AgentOut


{- NOTE: this is the main-function used to drive this YAMPA solution: it utilizes IORef to transfer the output back
         to input, because this simulation needs a feedback. This feedback was implemented in this way but can
         be achieved also without IORef using dpswitch - see the according file -}
{-
main :: IO ()
main = do
    let g = mkStdGen 42 -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g 5 heroDistribution
    agentsRef <- newIORef as
    reactimate (Main.init as) (input agentsRef) (output agentsRef) (YampaBack.process as)
    Front.shutdown

init :: [Agent.AgentState] -> IO [Agent.AgentState]
init initAs = do
    return initAs

input :: IORef [Agent.AgentState] -> Bool -> IO (DTime, Maybe [Agent.AgentState])
input ref _ = do
    as <- readIORef ref
    return (0.5, Just as)

output :: IORef [Agent.AgentState] -> Bool -> [Agent.AgentOut] -> IO Bool
output ref _ aos = do
    let as = map agentOutState aos
    writeIORef ref as
    winOpened <- Front.renderFrame aos
    return $ not winOpened
-}
----------------------------------------------------------------------------------------------------------------------

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
        aos <- par route sfs -< as
        returnA -< aos
    where
        sfs = replicate (length initAs) activeAgent

route :: [Agent.AgentState] -> [sf] -> [(Agent.AgentIn, sf)]
route as sfs = zip ains sfs
    where
        ains = Agent.agentInFromAgents as

activeAgent :: ActiveAgent
activeAgent = proc agentIn ->
    do
        t <- time -< ()
        d <- derivative -< t :: Double
        stepWidth <- (* Agent.agentSpeedPerTimeUnit) ^<< derivative -< t
        --let ao = trace ("time=" ++ (show t) ++ ", derivative=" ++ (show d) ++ "stepwidth=" ++ (show stepWidth)) Agent.agentStep agentIn stepWidth a
        returnA -< Agent.agentStep stepWidth agentIn

