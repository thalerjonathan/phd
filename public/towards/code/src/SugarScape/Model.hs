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

  , SugarScapeParams (..)
  , mkSugarScapeParams
  , mkParamsAnimationII_1
  , mkParamsAnimationII_2
  , mkParamsCarryingCapacity

  , maxSugarCapacityCell
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
  , sugAgSugarMetab       :: Int              -- this amount of sugar will be consumed by the agent in each time-step
  , sugAgVision           :: Int                 -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete
  , sugAgSugarLevel       :: Int              -- the current sugar holdings of the agent, if 0 then the agent starves to death
    } deriving (Show, Eq)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord    :: Discrete2dCoord
  , sugObsVision   :: Int
  } deriving (Show, Eq)

data SugEnvCellOccupier = SugEnvCellOccupier 
  { sugEnvOccId     :: AgentId
  , sugEnvOccWealth :: Int
  } deriving (Show, Eq)

data SugEnvCell = SugEnvCell 
  { sugEnvSugarCapacity :: Int
  , sugEnvSugarLevel    :: Int
  , sugEnvOccupier      :: Maybe SugEnvCellOccupier
  } deriving (Show, Eq)

type SugEnvironment = Discrete2d SugEnvCell

type SugAgentMonad g  = StateT SugEnvironment (Rand g)
type SugAgentMonadT g = AgentT (SugAgentMonad g)

type SugAgent g     = Agent    (SugAgentMonad g) SugAgentObservable
type SugAgentDef g  = AgentDef (SugAgentMonad g) SugAgentObservable
type SugAgentIn     = AgentIn 
type SugAgentOut g  = AgentOut (SugAgentMonad g) SugAgentObservable
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- SUGARSCAPE PARAMETERS
------------------------------------------------------------------------------------------------------------------------
maxSugarCapacityCell :: Int
maxSugarCapacityCell = 4

data SugarScapeParams = SugarScapeParams 
  { sgAgentCount           :: Int
  , spSugarGrowBackRate    :: Int            -- negative value means G_inf: regrow to max in next step
  , spSugarEndowmentRange  :: (Int, Int)
  , spSugarMetabolismRange :: (Int, Int)
  , spVisionRange          :: (Int, Int)
  }

mkSugarScapeParams :: SugarScapeParams
mkSugarScapeParams = SugarScapeParams {
    sgAgentCount           = 0
  , spSugarGrowBackRate    = 0
  , spSugarEndowmentRange  = (0, 0)
  , spSugarMetabolismRange = (0, 0)
  , spVisionRange          = (0, 0) 
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
mkParamsAnimationII_1 :: SugarScapeParams
mkParamsAnimationII_1 = mkSugarScapeParams {
    sgAgentCount           = 400         -- page 28
  , spSugarGrowBackRate    = -1        -- regrow to max immediately
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)  -- NOTE: specified where? 1 - 4
  , spVisionRange          = (1, 6)      -- NOTE: set to 1-6 on page 24
  }

mkParamsAnimationII_2 :: SugarScapeParams
mkParamsAnimationII_2 = mkSugarScapeParams {
    sgAgentCount           = 400         -- page 28
  , spSugarGrowBackRate    = 1         -- regrow by 1 unit per step
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)  -- NOTE: specified where? 1 - 4
  , spVisionRange          = (1, 6)      -- NOTE: set to 1-6 on page 24
  }

mkParamsCarryingCapacity :: SugarScapeParams
mkParamsCarryingCapacity = mkSugarScapeParams {
    sgAgentCount           = 400         -- 400 agents only
  , spSugarGrowBackRate    = 1         -- regrow by 1 unit per step
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)  -- NOTE: specified where? 1 - 4
  , spVisionRange          = (1, 6)      -- NOTE: set to 1-6 on page 24
  }

------------------------------------------------------------------------------------------------------------------------

{-
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

-- NOTE: specified where? 1 - 4
sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 4.0)

visionRange :: (Int, Int)
visionRange = visionRangeStandard
-- NOTE: set to 1-6 on page 24
visionRangeStandard :: (Int, Int)
visionRangeStandard = (1, 6)
-}