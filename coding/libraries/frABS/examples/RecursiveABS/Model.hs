{-# LANGUAGE Arrows #-}
module RecursiveABS.Model (
    RecursiveABSMsg (..),
    RecursiveABSAgentState,

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

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data RecursiveABSMsg = Hello Int deriving (Eq, Show)
type RecursiveABSAgentState = Int

type RecursiveABSEnvironment = Int

type RecursiveABSAgentDef = AgentDef RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvironment
type RecursiveABSAgentBehaviour = AgentBehaviour RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvironment
type RecursiveABSAgentIn = AgentIn RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvironment
type RecursiveABSAgentOut = AgentOut RecursiveABSAgentState RecursiveABSMsg RecursiveABSEnvironment
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
        recursiveStates = map (aoState . fst) recursiveOuts

        aout' = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (recursiveABSRandomizeCounter aout)
        aoutRec = recursive allowRecursionToOthers aout'

        aoutSelected = trace ("agent " ++ (show $ aiId ain) ++ ":  has recursiveOuts: " ++ (show recursiveStates)) (recursiveOuts !! 0)
        aoutUnRec = unrecursive $ fst aoutSelected

recursiveABSActNonRec :: RecursiveABSAgentOut -> RecursiveABSAgentIn -> RecursiveABSAgentOut
recursiveABSActNonRec aout ain
    | recInitAllowed ain = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is allowed, requests recursion, generating state " ++ (show $ aoState aoutRec)) aoutRec
    | otherwise = trace ("agent " ++ (show $ aiId ain) ++ ": recursion is forbidden, generating state " ++ (show $ aoState aout')) aout'
    where
        aout' = recursiveABSRandomizeCounter aout
        aoutRec = recursive allowRecursionToOthers aout'

recursiveABSRandomizeCounter :: RecursiveABSAgentOut -> RecursiveABSAgentOut
recursiveABSRandomizeCounter aout = aout1
    where
        (randInt, aout0) = agentRandomRange randomRangeCounter aout
        aout1 = setAgentState randInt aout0

recursiveABSAgentBehaviour :: RecursiveABSAgentBehaviour
recursiveABSAgentBehaviour = proc (ain, e) ->
    do
        let ao = agentOutFromIn ain
        t <- time -< 0.0
        let ao' = trace ("agent " ++ (show $ aiId ain) ++ ": at time = " ++ (show t) ++ " has " ++ (show $ aoState ao)) (recursiveABSStep ao ain)
        returnA -< (ao', e)