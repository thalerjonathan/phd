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
  , AgentDistribution (..)
  , mkSugarScapeParams

  , mkParamsAnimationII_1
  , mkParamsAnimationII_2
  , mkParamsAnimationII_3
  , mkParamsAnimationII_4
  , mkParamsAnimationII_6

  , mkParamsTerracing
  , mkParamsCarryingCapacity
  , mkParamsWealthDistr

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
  , sugAgSugarMetab       :: Int               -- integer because discrete, otherwise no exact replication possible
  , sugAgVision           :: Int
  , sugAgSugarLevel       :: Double            -- floating point because regrow-rate can be set to floating point values
  , sugAgAge              :: Int
  , sugAgMaxAge           :: Int
  } deriving (Show, Eq)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord    :: Discrete2dCoord
  , sugObsVision   :: Int
  , sugObsAge      :: Int
  , sugObsSugLvl   :: Double
  , sugObsSugMetab :: Int
  } deriving (Show, Eq)

data SugEnvCellOccupier = SugEnvCellOccupier 
  { sugEnvOccId     :: AgentId
  } deriving (Show, Eq)

data SugEnvCell = SugEnvCell 
  { sugEnvSugarCapacity :: Double
  , sugEnvSugarLevel    :: Double
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

liveForever :: (Int, Int)
liveForever = (1000000, 1000000)

data AgentDistribution = Scatter | Corner Discrete2dCoord

data SugarScapeParams = SugarScapeParams 
  { sgAgentCount           :: Int
  , sgAgentDistribution    :: AgentDistribution
  , spSugarGrowBackRate    :: Double            -- negative value means G_inf: regrow to max in next step, floating point to allow grow-back of less than 1
  , spSugarEndowmentRange  :: (Int, Int)
  , spSugarMetabolismRange :: (Int, Int)
  , spVisionRange          :: (Int, Int)
  , spReplaceAgents        :: Bool           -- replacement rule R_[a, b] on/off
  , spMaxAge               :: (Int, Int)
  }

mkSugarScapeParams :: SugarScapeParams
mkSugarScapeParams = SugarScapeParams {
    sgAgentCount           = 0
  , sgAgentDistribution    = Scatter
  , spSugarGrowBackRate    = 0
  , spSugarEndowmentRange  = (0, 0)
  , spSugarMetabolismRange = (0, 0)
  , spVisionRange          = (0, 0)
  , spReplaceAgents        = False
  , spMaxAge               = (0, 0)
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- Social Evolution with immediate regrow, page 27
mkParamsAnimationII_1 :: SugarScapeParams 
mkParamsAnimationII_1 = mkSugarScapeParams {
    sgAgentCount           = 400     -- page 28
  , sgAgentDistribution    = Scatter
  , spSugarGrowBackRate    = -1      -- regrow to max immediately
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)  -- NOTE: specified where? 1 - 4
  , spVisionRange          = (1, 6)  -- NOTE: set to 1-6 on page 24
  , spReplaceAgents        = False   -- no replacing of died agents
  , spMaxAge               = liveForever  -- agents dont die of age in this case
  }
-- terracing phenomenon as described on page 28
mkParamsTerracing :: SugarScapeParams 
mkParamsTerracing = mkParamsAnimationII_1

-- Social Evolution with regrow rate of 1, page 29
mkParamsAnimationII_2 :: SugarScapeParams
mkParamsAnimationII_2 = mkSugarScapeParams {
    sgAgentCount           = 400     -- page 28
  , sgAgentDistribution    = Scatter
  , spSugarGrowBackRate    = 1       -- regrow by 1 unit per step
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)
  , spVisionRange          = (1, 6)
  , spReplaceAgents        = False        -- no replacing of died agents
  , spMaxAge               = liveForever  -- agents dont die of age in this case
  }
-- carrying capacity property as described on page 30
mkParamsCarryingCapacity :: SugarScapeParams
mkParamsCarryingCapacity = mkParamsAnimationII_2

-- Wealth Distribution page 34
mkParamsAnimationII_3 :: SugarScapeParams
mkParamsAnimationII_3 = mkSugarScapeParams {
    sgAgentCount           = 250        -- page 33
  , sgAgentDistribution    = Scatter
  , spSugarGrowBackRate    = 1          -- page 33
  , spSugarEndowmentRange  = (5, 25)    -- page 33
  , spSugarMetabolismRange = (1, 4)
  , spVisionRange          = (1, 6)
  , spReplaceAgents        = True       -- page 33
  , spMaxAge               = (60, 100)  -- page 33
  }
-- wealth distribution as described on page 32-37
mkParamsAnimationII_4 :: SugarScapeParams
mkParamsAnimationII_4 = mkParamsAnimationII_3 -- same as G_1, M, R_60,100 => same as Animiation II-3
-- wealth distribution as described on page 32-37
mkParamsWealthDistr :: SugarScapeParams
mkParamsWealthDistr = mkParamsAnimationII_3 -- same as G_1, M, R_60,100 => same as Animiation II-3

-- Migration as described on page 42 and 43 in Animation II-6
mkParamsAnimationII_6 :: SugarScapeParams
mkParamsAnimationII_6 = mkSugarScapeParams {
    sgAgentCount           = 300              -- 300 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , sgAgentDistribution    = Corner (20, 20)
  , spSugarGrowBackRate    = 0.5              -- 0.5 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , spSugarEndowmentRange  = (5, 25)
  , spSugarMetabolismRange = (1, 4)
  , spVisionRange          = (1, 10)          -- increase vision to 10, see page 42, we suggest to to 15 to make the waves really prominent
  , spReplaceAgents        = False            -- agents in migration experiment are not replaced
  , spMaxAge               = liveForever      -- agents in Migration experiment do not die of age
  }
------------------------------------------------------------------------------------------------------------------------