{-# LANGUAGE Arrows #-}
module RecursiveABS.Model (
    RecursiveABSMsg (..),
    RecursiveABSAgentState,

    RecursiveABSEnvLink,
    RecursiveABSEnvCell,
    RecursiveABSEnvironment,

    RecursiveABSAgentDef,
    RecursiveABSAgentBehaviour,
    RecursiveABSAgentIn,
    RecursiveABSAgentOut,

    randomRangeCounter,
    allowRecursionToOthers,

    recursiveABSAgentBehaviour
  ) where

import FRP.FrABS

import FRP.Yampa

import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data RecursiveABSMsg = Hello Int deriving (Eq, Show)
type RecursiveABSAgentState = Int

type RecursiveABSEnvLink = ()
type RecursiveABSEnvCell = Int
type RecursiveABSEnvironment = Environment RecursiveABSEnvCell RecursiveABSEnvLink

type RecursiveABSAgentDef = AgentDef RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvCell RecursiveABSEnvLink
type RecursiveABSAgentBehaviour = AgentBehaviour RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvCell RecursiveABSEnvLink
type RecursiveABSAgentIn = AgentIn RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvCell RecursiveABSEnvLink
type RecursiveABSAgentOut = AgentOut RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvCell RecursiveABSEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)

allowRecursionToOthers :: Bool
allowRecursionToOthers = False
------------------------------------------------------------------------------------------------------------------------

recursiveABSStep :: RecursiveABSAgentOut -> RecursiveABSAgentIn -> RecursiveABSAgentOut
recursiveABSStep aout ain
    | isRecursive ain = trace ("agent " ++ (show $ aiId ain) ++ ": RECURSIVE with state: " ++ (show $ aoState aout)) recursiveABSActRecursive aout ain
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": NORMAL with state: " ++ (show $ aoState aout)) recursiveABSActNonRec aout ain

recursiveABSActRecursive :: RecursiveABSAgentOut -> RecursiveABSAgentIn -> RecursiveABSAgentOut
recursiveABSActRecursive aout ain
    | length recursiveOuts < 2 = trace ("agent " ++ (show $ aiId ain) ++ ":  continuing recursive simulation, generating state " ++ (show $ aoState aoutRec)) aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ":  stopping recursive simulation, returning state " ++ (show $ aoState aoutUnRec)) aoutUnRec
    where
        recursiveOuts = fromEvent $ aiRec ain
        recursiveStates = map aoState recursiveOuts

        aout' = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (recursiveABSRandomizeCounter aout)
        aoutRec = recursive aout' allowRecursionToOthers

        aoutSelected = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (recursiveOuts !! 0)
        aoutUnRec = unrecursive aoutSelected

recursiveABSActNonRec :: RecursiveABSAgentOut -> RecursiveABSAgentIn -> RecursiveABSAgentOut
recursiveABSActNonRec aout ain
    | recInitAllowed ain = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is allowed, requests recursion, generating state " ++ (show $ aoState aoutRec)) aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is forbidden, generating state " ++ (show $ aoState aout')) aout'
    where
        aout' = recursiveABSRandomizeCounter aout
        aoutRec = recursive aout' allowRecursionToOthers

recursiveABSRandomizeCounter :: RecursiveABSAgentOut -> RecursiveABSAgentOut
recursiveABSRandomizeCounter aout = aout1
    where
        (randInt, aout0) = drawRandomRangeFromAgent aout randomRangeCounter
        aout1 = setDomainState aout0 randInt

recursiveABSAgentBehaviour :: RecursiveABSAgentBehaviour
recursiveABSAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        t <- time -< 0.0
        let ao' = trace ("agent " ++ (show $ aiId ain) ++ ": at time = " ++ (show t) ++ " has " ++ (show $ aoState ao)) (recursiveABSStep ao ain)
        returnA -<ao'