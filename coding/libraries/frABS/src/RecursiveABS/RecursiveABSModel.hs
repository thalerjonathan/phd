{-# LANGUAGE Arrows #-}
module RecursiveABS.RecursiveABSModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data MetaABSMsg = Hello Int deriving (Eq, Show)

data MetaABSAgentState = MetaABSAgentState {
    mabsCounter :: Int
} deriving (Show)

type MetaABSEnvCell = Int
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

allowRecursionToOthers :: Bool
allowRecursionToOthers = False
------------------------------------------------------------------------------------------------------------------------

metaABSStep :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSStep aout ain
    | isRecursive ain = trace ("agent " ++ (show $ aiId ain) ++ ": RECURSIVE with state: " ++ (show $ aoState aout)) metaABSActRecursive aout ain
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": NORMAL with state: " ++ (show $ aoState aout)) metaABSActNonRec aout ain

metaABSActRecursive :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSActRecursive aout ain
    | length recursiveOuts < 2 = trace ("agent " ++ (show $ aiId ain) ++ ":  continuing recursive simulation, generating state " ++ (show $ aoState aoutRec)) aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ":  stopping recursive simulation, returning state " ++ (show $ aoState aoutUnRec)) aoutUnRec
    where
        recursiveOuts = fromEvent $ aiRec ain
        recursiveStates = map aoState recursiveOuts

        aout' = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (metaABSRandomizeCounter aout)
        aoutRec = recursive aout' allowRecursionToOthers

        aoutSelected = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (recursiveOuts !! 0)
        aoutUnRec = unrecursive aoutSelected

metaABSActNonRec :: MetaABSAgentOut -> MetaABSAgentIn -> MetaABSAgentOut
metaABSActNonRec aout ain
    | recInitAllowed ain = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is allowed, requests recursion, generating state " ++ (show $ aoState aoutRec)) aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is forbidden, generating state " ++ (show $ aoState aout')) aout'
    where
        aout' = metaABSRandomizeCounter aout
        aoutRec = recursive aout' allowRecursionToOthers

metaABSRandomizeCounter :: MetaABSAgentOut -> MetaABSAgentOut
metaABSRandomizeCounter aout = aout1
    where
        (randInt, aout0) = drawRandomRangeFromAgent aout randomRangeCounter
        aout1 = updateState aout0 (\s -> s { mabsCounter = randInt } )

metaABSAgentBehaviour :: MetaABSAgentBehaviour
metaABSAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        t <- time -< 0.0
        let ao' = trace ("agent " ++ (show $ aiId ain) ++ ": at time = " ++ (show t) ++ " has " ++ (show $ aoState ao)) (metaABSStep ao ain)
        returnA -<ao'