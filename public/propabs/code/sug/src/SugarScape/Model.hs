module SugarScape.Model 
  ( SugAgentState (..)
  , SugAgentObservable (..)

  , SugEnvCellOccupier (..)
  , SugEnvCell (..)

  , SugAgentMonad
  , SugAgentMonadT

  , SugEnvironment

  , SugAgent
  , SugAgentDef
  , SugAgentIn
  , SugAgentOut

  , sugarGrowbackUnits
  , sugarCapacityRange
  , sugarEndowmentRange
  , sugarEndowmentRangeStandard
  , sugarMetabolismRange
  , visionRange
  , visionRangeStandard
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.AgentMonad
import SugarScape.Discrete

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SugAgentState = SugAgentState 
  { sugAgCoord            :: Discrete2dCoord
  , sugAgSugarMetab       :: Double              -- this amount of sugar will be consumed by the agent in each time-step
  , sugAgVision           :: Int                 -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete
  , sugAgSugarLevel       :: Double              -- the current sugar holdings of the agent, if 0 then the agent starves to death
  , sugAgSugarInit        :: Double              -- agent is fertile only when its sugarlevel is GE than its initial endowment
    } deriving (Show)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord    :: Discrete2dCoord
  , sugObsVision   :: Int
  } deriving (Show)

data SugEnvCellOccupier = SugEnvCellOccupier 
  { sugEnvOccId     :: AgentId
  , sugEnvOccWealth :: Double
  } deriving (Show)

data SugEnvCell = SugEnvCell 
  { sugEnvSugarCapacity :: Double
  , sugEnvSugarLevel    :: Double
  , sugEnvOccupier      :: Maybe SugEnvCellOccupier
  } deriving (Show)

type SugEnvironment = Discrete2d SugEnvCell

type SugAgentMonad g  = StateT SugEnvironment (Rand g)
type SugAgentMonadT g = AgentT (SugAgentMonad g)

type SugAgent g     = Agent    (SugAgentMonad g) SugAgentObservable
type SugAgentDef g  = AgentDef (SugAgentMonad g) SugAgentObservable
type SugAgentIn     = AgentIn 
type SugAgentOut g  = AgentOut (SugAgentMonad g) SugAgentObservable
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- NOTE: < 0 is treated as grow back to max
sugarGrowbackUnits :: Double
sugarGrowbackUnits = 1.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (0.0, 4.0)

sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = sugarEndowmentRangeStandard
-- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
sugarEndowmentRangeStandard :: (Double, Double)
sugarEndowmentRangeStandard = (5.0, 25.0)

sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 5.0)

visionRange :: (Int, Int)
visionRange = visionRangeStandard
-- NOTE: set to 1-6 on page 24
visionRangeStandard :: (Int, Int)
visionRangeStandard = (1, 6)
