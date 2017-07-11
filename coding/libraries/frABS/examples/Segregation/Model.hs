module Segregation.Model (
    SegParty (..),
    SegMsg,

    SegMoveStrategy (..),
    SegSelectionStrategy (..),
    SegOptStrategy (..),

    SegAgentState (..),

    SegEnvLink,
    SegEnvCell,
    SegEnvironment,

    SegAgentDef,
    SegAgentBehaviour,
    SegAgentIn,
    SegAgentOut,

    similarityWanted,
    density,
    redGreenDist,
    localMovementRadius,
    randomSearchFreeCellRetries,
    randomSearchOptRetries,
    movementStrategy,
    selectionStrategy,
    optimizingStrategy,
    futureOptimizing
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SegParty = Red | Green deriving (Eq, Show)
type SegMsg = ()    -- Agents are not communicating in Schelling Segregation

data SegMoveStrategy = MoveLocal Int | MoveGlobal deriving (Eq)
data SegSelectionStrategy = SelectNearest | SelectRandom Int Int deriving (Eq)
data SegOptStrategy = OptNone
                        | OptSimilaritySatisfied
                        | OptSimilarityIncreasing
                            deriving (Eq)

data SegAgentState = SegAgentState {
    segParty :: SegParty,
    segSatisfactionLevel :: Double,
    segSimilarityWanted :: Double
} deriving (Show)

type SegEnvLink = ()
type SegEnvCell = Maybe SegParty
type SegEnvironment = Environment SegEnvCell SegEnvLink

type SegAgentDef = AgentDef SegAgentState SegMsg SegEnvCell SegEnvLink
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg SegEnvCell SegEnvLink
type SegAgentIn = AgentIn SegAgentState SegMsg SegEnvCell SegEnvLink
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvCell SegEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
similarityWanted :: Double
similarityWanted = 0.8

density :: Double
density = 0.8

redGreenDist :: Double
redGreenDist = 0.5

localMovementRadius :: Int
localMovementRadius = 5

randomSearchFreeCellRetries :: Int
randomSearchFreeCellRetries = 4

randomSearchOptRetries :: Int
randomSearchOptRetries = 1

movementStrategy :: SegMoveStrategy
movementStrategy = MoveGlobal -- MoveLocal localMovementRadius

selectionStrategy :: SegSelectionStrategy
selectionStrategy = SelectRandom randomSearchOptRetries randomSearchFreeCellRetries -- SelectNearest -- SelectRandom randomSearchOptRetries randomSearchFreeCellRetries

optimizingStrategy :: SegOptStrategy
optimizingStrategy = OptSimilaritySatisfied -- OptNone -- OptSimilaritySatisfied -- OptSimilarityIncreasing 

futureOptimizing :: Bool
futureOptimizing = True
------------------------------------------------------------------------------------------------------------------------