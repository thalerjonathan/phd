module Segregation.Model (
    SegParty (..),
    SegMsg,

    SegMoveStrategy (..),
    SegSelectionStrategy (..),
    SegOptStrategy (..),

    SegAgentState (..),

    SegEnvCell,
    SegEnvironment,

    SegAgentDef,
    SegAgentBehaviour,
    SegAgentIn,
    SegAgentOut,

    SegAgentPureBehaviour,
    
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
    segSimilarityWanted :: Double,
    segCoord :: Discrete2dCoord
} deriving (Show)

type SegEnvCell = Maybe SegParty
type SegEnvironment = Discrete2d SegEnvCell

type SegAgentDef = AgentDef SegAgentState SegMsg SegEnvironment
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg SegEnvironment
type SegAgentIn = AgentIn SegAgentState SegMsg SegEnvironment
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvironment

type SegAgentPureBehaviour = AgentPureBehaviour SegAgentState SegMsg SegEnvironment
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
movementStrategy = MoveLocal localMovementRadius -- MoveGlobal -- MoveLocal localMovementRadius

selectionStrategy :: SegSelectionStrategy
selectionStrategy = SelectNearest -- SelectRandom randomSearchOptRetries randomSearchFreeCellRetries -- SelectNearest -- SelectRandom randomSearchOptRetries randomSearchFreeCellRetries

optimizingStrategy :: SegOptStrategy
optimizingStrategy = OptNone -- OptSimilaritySatisfied -- OptNone -- OptSimilaritySatisfied -- OptSimilarityIncreasing 

futureOptimizing :: Bool
futureOptimizing = True
------------------------------------------------------------------------------------------------------------------------