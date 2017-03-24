{-# LANGUAGE Arrows #-}
module MetaABS.MetaABSModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data MetaABSMsg = Hello Int deriving (Eq, Show)

data MetaABSAgentState = MetaABSAgentState {
    mabsCounter :: Int,
    mabsRng :: StdGen
} deriving (Show)

type MetaABSEnvCell = ()
type MetaABSEnvironment = Environment MetaABSEnvCell

type MetaABSAgentDef = AgentDef MetaABSAgentState MetaABSMsg MetaABSEnvCell
type MetaABSAgentBehaviour = AgentBehaviour MetaABSAgentState MetaABSMsg MetaABSEnvCell
type MetaABSAgentIn = AgentIn MetaABSAgentState MetaABSMsg MetaABSEnvCell
type MetaABSAgentOut = AgentOut MetaABSAgentState MetaABSMsg MetaABSEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)
------------------------------------------------------------------------------------------------------------------------

metaABSStep :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSStep aout ain
    | isRecursive ain = trace ("agent " ++ (show $ aiId ain) ++ ": isRecursive") metaABSActRecursive aout ain
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": isNormal") metaABSActNonRec aout ain

metaABSActRecursive :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSActRecursive aout ain
    | length recursiveOuts < 2 = trace ("agent " ++ (show $ aiId ain) ++ ": continuing recursive simulation") aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": stopping recursive simulation") aoutUnRec
    where
        recursiveOuts = fromEvent $ aiRec ain
        recursiveStates = map aoState recursiveOuts

        aout' = trace ("agent " ++ (show $ aiId ain) ++ ": has recursiveOuts: " ++ (show recursiveStates)) (metaABSRandomizeCounter aout)
        aoutRec = recursive aout'

        aoutSelected = trace ("agent " ++ (show $ aiId ain) ++ ": has recursiveOuts: " ++ (show recursiveStates)) (recursiveOuts !! 0)
        aoutUnRec = unrecursive aoutSelected

metaABSActNonRec :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSActNonRec aout ain = trace ("agent " ++ (show $ aiId ain) ++ ": requests recursion") aoutRec
    where
        aout' = metaABSRandomizeCounter aout
        aoutRec = recursive aout'

metaABSRandomizeCounter :: MetaABSAgentOut -> MetaABSAgentOut
metaABSRandomizeCounter aout = aout'
    where
        g = mabsRng $ aoState aout
        (randInt, g') = randomR randomRangeCounter g
        aout' = updateState aout (\s -> s { mabsCounter = randInt, mabsRng = g' } )

metaABSAgentBehaviour :: MetaABSAgentBehaviour
metaABSAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        t <- time -< 0.0
        returnA -< trace ("agent " ++ (show $ aiId ain) ++ ": time = " ++ (show t)) (metaABSStep ao ain)