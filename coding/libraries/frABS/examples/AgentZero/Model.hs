module AgentZero.Model (
    AgentZeroMsg (..),
    AgentZeroAgentState (..),
    AgentZeroCellState (..),
    AgentZeroEnvCell (..),

    AgentZeroEnvironment (..),
    AgentZeroEnvironmentBehaviour,

    AgentZeroNetwork,
    AgentZeroWorldPatches,

    AgentZeroAgentDef,
    AgentZeroAgentBehaviour,
    AgentZeroAgentIn,
    AgentZeroAgentOut,

    extinctionRate,
    destructionRadius,
    sampleRadius,
    memorySize
  ) where

import           FRP.FrABS

import           FRP.Yampa

import           Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data AgentZeroMsg = Disposition Double deriving (Eq, Show)

data AgentZeroAgentState = AgentZeroAgentState {
  azAgentAffect       :: Double,
  azAgentLearningRate :: Double,
  azAgentLambda       :: Double,
  azAgentDelta        :: Double,
  azAgentThresh       :: Double,
  azAgentEventCount   :: Int,
  azAgentDispo        :: Double,
  azAgentProb         :: Double,
  azAgentMemory       :: [Double],
  azAgentCoord        :: Continuous2DCoord
} deriving (Show)

data AgentZeroCellState = Friendly | Attack | Dead deriving (Eq, Show)

data AgentZeroEnvCell = AgentZeroEnvCell {
  azCellState :: AgentZeroCellState,
  azCellShade :: Float
} deriving (Show)

data AgentZeroEnvironment = AgentZeroEnvironment {
  azAgentNetwork :: AgentZeroNetwork,
  azAgentSpace :: Continuous2d,
  azWorldPatches :: AgentZeroWorldPatches
}

type AgentZeroNetwork = Network Double
type AgentZeroWorldPatches = Discrete2d AgentZeroEnvCell
type AgentZeroEnvironmentBehaviour = EnvironmentBehaviour AgentZeroEnvironment

type AgentZeroAgentDef = AgentDef AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentBehaviour = AgentBehaviour AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentIn = AgentIn AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentOut = AgentOut AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
extinctionRate :: Double
extinctionRate = 1.0

destructionRadius :: Double
destructionRadius = 1.0

sampleRadius :: Double
sampleRadius = 1.0

memorySize :: Int
memorySize = 10
------------------------------------------------------------------------------------------------------------------------