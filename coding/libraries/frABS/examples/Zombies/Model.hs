module Zombies.Model (
    ZombiesMsg (..),
    ZombiesAgentState (..),

    ZombiesEnvironment (..),

    ZombiesNetwork,
    ZombiesPatch,
    ZombiesPatches,

    ZombiesAgentDef,
    ZombiesAgentBehaviour,
    ZombiesAgentIn,
    ZombiesAgentOut,

    humanCount,
    zombieCount,

    humanInitEnergyRange,
    gridDimensions,

    isHuman,
    isZombie
  ) where

import           FRP.FrABS

import           FRP.Yampa

import           Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data ZombiesMsg = Infect deriving (Eq, Show)

data ZombiesAgentState = 
    ZombiesState {
      zAgentCoord :: Continuous2DCoord
    }
  | HumanState {
      zAgentCoord :: Continuous2DCoord,
      zHumanEnergy :: Int
  } deriving (Show)

data ZombiesEnvironment = ZombiesEnvironment {
  zAgentNetwork :: ZombiesNetwork,
  zAgentSpace :: Continuous2d,
  zAgentPatches :: ZombiesPatches
}

type ZombiesNetwork = Network ()
type ZombiesPatch = Int
type ZombiesPatches = Discrete2d ZombiesPatch

type ZombiesAgentDef = AgentDef ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentBehaviour = AgentBehaviour ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentIn = AgentIn ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentOut = AgentOut ZombiesAgentState ZombiesMsg ZombiesEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
humanCount :: Int
humanCount = 100

zombieCount :: Int
zombieCount = 5

humanInitEnergyRange :: (Int, Int)
humanInitEnergyRange = (4, 10)

gridDimensions :: Discrete2dDimension
gridDimensions = (5, 5)
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------------------------------------------------------------------
isHuman :: ZombiesAgentState -> Bool
isHuman (HumanState {}) = True
isHuman _ = False

isZombie :: ZombiesAgentState -> Bool
isZombie (ZombiesState {}) = True
isZombie _ = False
------------------------------------------------------------------------------------------------------------------------