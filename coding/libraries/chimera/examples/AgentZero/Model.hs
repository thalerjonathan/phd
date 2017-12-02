module Model 
  (
    AgentZeroMsg (..)
  , AgentZeroAgentState (..)
  , AgentZeroCellState (..)
  , AgentZeroEnvCell (..)

  , AgentZeroEnvironment (..)
  , AgentZeroEnvironmentBehaviour

  , AgentZeroNetwork
  , AgentZeroWorldPatches

  , AgentZeroAgentDef
  , AgentZeroAgentBehaviour
  , AgentZeroAgentIn
  , AgentZeroAgentOut

  , extinctionRate
  , destructionRadius
  , sampleRadius
  , memorySize

  , movementSpeed
  ) where

import FRP.FrABS

-------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
-------------------------------------------------------------------------------
data AgentZeroMsg = Disposition Double deriving (Eq, Show)

data AgentZeroAgentState = AgentZeroAgentState 
  {
    azAgentAffect       :: Double
  , azAgentLearningRate :: Double
  , azAgentLambda       :: Double
  , azAgentDelta        :: Double
  , azAgentThresh       :: Double
  , azAgentEventCount   :: Int
  , azAgentDispo        :: Double
  , azAgentProb         :: Double
  , azAgentMemory       :: [Double]
  , azAgentCoord        :: Continuous2dCoord
  } deriving (Show)

data AgentZeroCellState = Friendly | Attack | Dead deriving (Eq, Show)

data AgentZeroEnvCell = AgentZeroEnvCell 
  {
    azCellState :: AgentZeroCellState
  , azCellShade :: Float
  } deriving (Show)

data AgentZeroEnvironment = AgentZeroEnvironment 
  {
    azAgentNetwork :: AgentZeroNetwork
  , azAgentSpace :: Continuous2dEmpty
  , azWorldPatches :: AgentZeroWorldPatches
  }

type AgentZeroNetwork               = Network Double
type AgentZeroWorldPatches          = Discrete2d AgentZeroEnvCell
type AgentZeroEnvironmentBehaviour  = EnvironmentBehaviour AgentZeroEnvironment

type AgentZeroAgentDef        = AgentDef AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentBehaviour  = AgentBehaviour AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentIn         = AgentIn AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
type AgentZeroAgentOut        = AgentOut AgentZeroAgentState AgentZeroMsg AgentZeroEnvironment
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MODEL-PARAMETERS
-------------------------------------------------------------------------------
extinctionRate :: Double
extinctionRate = 1.0

destructionRadius :: Double
destructionRadius = 1.0

sampleRadius :: Double
sampleRadius = 5.0

memorySize :: Int
memorySize = 10

movementSpeed :: Double
movementSpeed = 1.0
-------------------------------------------------------------------------------