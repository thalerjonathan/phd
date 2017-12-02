module Zombies.Model (
    ZombiesMsg (..),
    Role (..),
    ZombiesAgentState (..),

    ZombiesEnvironment,

    ZombiesNetwork,
    ZombiesPatch,
    ZombiesPatches,

    ZombiesAgentDef,
    ZombiesAgentBehaviour,
    ZombiesAgentIn,
    ZombiesAgentOut,
    ZombiesAgentObservable,
    
    humanCount,
    zombieCount,
    zombieSpeed,
    humanSpeed,

    humanInitEnergyRange,
    gridDimensions,

    sortPatchesByZombies,
    sortPatchesByHumans,

    humansOnPatch,
    addHuman,
    removeHuman,
    incZombie,
    decZombie,

    isHuman,
    isZombie
  ) where

import FRP.FrABS

import Data.List

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data ZombiesMsg = Infect deriving (Eq, Show)
data Role = Human | Zombie deriving (Eq, Show)

data ZombiesAgentState = 
    ZombiesState {
      zAgentRole :: Role,
      zAgentCoord :: Continuous2dCoord
    }
  | HumanState {
      zAgentRole :: Role,
      zAgentCoord :: Continuous2dCoord,
      zHumanEnergyLevel :: Int,
      zHumanEnergyInit :: Int
  } deriving (Show)

type ZombiesPatch = ([AgentId], Int)  -- fst: agentids of human on this patch, snd number of zombies on this patch

type ZombiesNetwork = Network ()
type ZombiesPatches = Discrete2d ZombiesPatch
type ZombiesEnvironment = (Continuous2dEmpty, ZombiesPatches, ZombiesNetwork)

type ZombiesAgentDef = AgentDef ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentBehaviour = AgentBehaviour ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentIn = AgentIn ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentOut = AgentOut ZombiesAgentState ZombiesMsg ZombiesEnvironment
type ZombiesAgentObservable = AgentObservable ZombiesAgentState
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
humanCount :: Int
humanCount = 200

zombieCount :: Int
zombieCount = 5

humanInitEnergyRange :: (Int, Int)
humanInitEnergyRange = (4, 10)

gridDimensions :: Discrete2dDimension
gridDimensions = (50, 50)

zombieSpeed :: Double
zombieSpeed = 1.0

humanSpeed :: Double
humanSpeed = 2.0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- UTILITIES
------------------------------------------------------------------------------------------------------------------------
sortPatchesByZombies :: Discrete2dCell ZombiesPatch -> Discrete2dCell ZombiesPatch -> Ordering
sortPatchesByZombies (_, (_, z1)) (_, (_, z2)) = compare z1 z2

sortPatchesByHumans :: Discrete2dCell ZombiesPatch -> Discrete2dCell ZombiesPatch -> Ordering
sortPatchesByHumans (_, p1) (_, p2) = compare (humansOnPatch p1) (humansOnPatch p2)

humansOnPatch :: ZombiesPatch -> Int
humansOnPatch = length . fst

addHuman :: AgentId -> ZombiesPatch -> ZombiesPatch
addHuman aid (hs, z) = (aid : hs, z)

removeHuman :: AgentId -> ZombiesPatch -> ZombiesPatch
removeHuman aid (hs, z) = (delete aid hs, z)

incZombie :: ZombiesPatch -> ZombiesPatch
incZombie (h, z) = (h, z+1)

decZombie :: ZombiesPatch -> ZombiesPatch
decZombie (h, z) = (h, z-1)

isHuman :: ZombiesAgentState -> Bool
isHuman s = zAgentRole s == Human

isZombie :: ZombiesAgentState -> Bool
isZombie s = zAgentRole s == Zombie
------------------------------------------------------------------------------------------------------------------------