module SugarScape.Model 
  ( AgentGender (..)
  , CultureTag
  , AgentTribe (..)
  , TradeInfo (..)
  
  , SugAgentState (..)
  , SugAgentObservable (..)

  , SugEnvSiteOccupier (..)
  , SugEnvSite (..)

  , TradingRefuse (..)
  , TradingReply (..)
  , SugEvent (..)
  
  , SugAgentMonad
  , SugAgentMonadT

  , SugEnvironment

  , SugAgentMSF
  , SugAgentDef
  , SugAgentOut

  , AgentAgeSpan (..)

  , SugarScapeParams (..)
  , AgentDistribution (..)
  , Regrow (..)
  , PolutionFormation (..)

  , maxSugarCapacitySite
  , maxSpiceCapacitySite
  , sugarscapeDimensions
  , envSpec

  , mkSugarScapeParams

  , mkParamsAnimationII_1
  , mkParamsAnimationII_2
  , mkParamsAnimationII_3
  , mkParamsAnimationII_4
  , mkParamsAnimationII_5
  , mkParamsAnimationII_6
  , mkParamsAnimationII_7
  , mkParamsAnimationII_8

  , mkParamsAnimationIII_1
  , mkParamsFigureIII_3
  , mkParamsFigureIII_4
  , mkParamsFigureIII_7
  , mkParamsAnimationIII_4
  , mkParamsAnimationIII_6
  , mkParamsAnimationIII_7
  , mkParamsFigureIII_8
  , mkParamsAnimationIII_9
  , mkParamsAnimationIII_10
  , mkParamsAnimationIII_11
  , mkParamsAnimationIII_14
  , mkParamsAnimationIII_15

  , mkParamsAnimationIV_1
  , mkParamsFigureIV_3
  , mkParamsFigureIV_4
  , mkParamsFigureIV_5
  , mkParamsFigureIV_6
  , mkCarryingCapacityWithSpice
  , mkParamsFigureIV_7
  , mkParamsFigureIV_8
  , mkParamsFigureIV_9
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Interface
import SugarScape.Common
import SugarScape.Discrete

------------------------------------------------------------------------------------------------------------------------
-- AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data AgentGender = Male | Female deriving (Show, Eq)
type CultureTag  = [Bool]
data AgentTribe  = Blue | Red deriving (Show, Eq)
data TradeInfo   = TradeInfo Double Double Double AgentId deriving (Show, Eq) -- price, sugar, spice, trade-partner

data SugAgentState = SugAgentState 
  { sugAgCoord        :: !Discrete2dCoord
  , sugAgSugarMetab   :: !Int               -- integer because discrete, otherwise no exact replication possible
  , sugAgVision       :: !Int
  , sugAgSugarLevel   :: !Double            -- floating point because regrow-rate can be set to floating point values
  , sugAgAge          :: !Int
  , sugAgMaxAge       :: !(Maybe Int)
  
  -- Chapter III properties
  , sugAgGender       :: !AgentGender
  , sugAgFertAgeRange :: !(Int, Int)        -- from, to
  , sugAgInitSugEndow :: !Double
  , sugAgChildren     :: ![AgentId]         -- list of all children the agent has given birth to (together with another agent of opposing sex)
  , sugAgCultureTag   :: !CultureTag
  , sugAgTribe        :: !AgentTribe
  
  -- Chapter IV properties
  , sugAgSpiceLevel   :: !Double            -- floating point because regrow-rate can be set to floating point values
  , sugAgInitSpiEndow :: !Double
  , sugAgSpiceMetab   :: !Int               -- integer because discrete, otherwise no exact replication possible
  } deriving (Show, Eq)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord      :: !Discrete2dCoord
  , sugObsVision     :: !Int
  , sugObsAge        :: !Int
  , sugObsSugLvl     :: !Double
  , sugObsSugMetab   :: !Int

  -- Chapter III properties
  , sugObsGender     :: !AgentGender
  , sugObsCultureTag :: !CultureTag
  , sugObsTribe      :: !AgentTribe

  -- Chapter IV properties
  , sugObsSpiLvl     :: !Double
  , sugObsSpiMetab   :: !Int
  , sugObsTrades     :: [TradeInfo]
  } deriving (Show, Eq)

data SugEnvSiteOccupier = SugEnvSiteOccupier 
  { sugEnvOccId          :: !AgentId
  , sugEnvOccSugarWealth :: !Double
  , sugEnvOccSpiceWealth :: !Double
  , sugEnvOccTribe       :: !AgentTribe
  , sugEnvOccMRS         :: !Double
  } deriving (Show, Eq)

data SugEnvSite = SugEnvSite 
  { sugEnvSiteSugarCapacity :: !Double
  , sugEnvSiteSugarLevel    :: !Double

  , sugEnvSiteSpiceCapacity :: !Double
  , sugEnvSiteSpiceLevel    :: !Double

  , sugEnvSitePolutionLevel :: !Double
  , sugEnvSiteOccupier      :: !(Maybe SugEnvSiteOccupier)
  } deriving (Show, Eq)

data TradingRefuse = NoWelfareIncrease  -- refuse trade because no increase in welfare
                   | MRSCrossover       -- refuse trade because MRS cross-over
                   deriving (Show, Eq)

data TradingReply = Accept         
                  | Refuse TradingRefuse 
                  deriving (Show, Eq)

data SugEvent = MatingRequest AgentGender
              | MatingReply (Maybe (Double, Double, Int, Int, CultureTag)) -- in case of acceptance: Just share of sugar, spice, metab, vision
              | MatingTx AgentId
              | MatingContinue

              | Inherit Double 

              | CulturalProcess CultureTag

              | KilledInCombat 

              | TradingOffer Double Double  -- offering agent sends MRS before and after trade so receiving agent can turn down if MRS cross-over
              | TradingReply TradingReply
              deriving (Show, Eq)

type SugEnvironment = Discrete2d SugEnvSite

type SugAgentMonad g  = StateT SugEnvironment (Rand g)
type SugAgentMonadT g = AgentT (SugAgentMonad g)

type SugAgentMSF g = AgentMSF (SugAgentMonad g) SugEvent SugAgentObservable
type SugAgentDef g = AgentDef (SugAgentMonad g) SugEvent SugAgentObservable
type SugAgentOut g = AgentOut (SugAgentMonad g) SugEvent SugAgentObservable
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- SUGARSCAPE PARAMETERS
------------------------------------------------------------------------------------------------------------------------
maxSugarCapacitySite :: Int
maxSugarCapacitySite = 4

maxSpiceCapacitySite :: Int
maxSpiceCapacitySite = 4

-- the sugarscape is 51x51 in our implementation
sugarscapeDimensions :: Discrete2dCoord
sugarscapeDimensions = (51, 51)

-- taken from Iain Weaver Sugarscape implementation
-- https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
-- http://ccl.northwestern.edu/netlogo/models/community/
envSpec :: [String]
envSpec =
  [ "111111111111111111111111111112222222222111111111111"
  , "111111111111111111111111111222222222222222111111111"
  , "111111111111111111111111112222222222222222221111111"
  , "111111111111111111111111122222222222222222222211111"
  , "111111111111111111111111222222222222222222222221111"
  , "111110000000111111111111222222222223332222222222111"
  , "111110000000001111111111222222223333333332222222111"
  , "111110000000000111111112222222333333333333222222211"
  , "111110000000000111111112222223333333333333322222211"
  , "111110000000000011111112222223333333333333332222221"
  , "111110000000000011111122222233333344444333333222221"
  , "111110000000000111111122222233333444444433333222221"
  , "111111000000000111111122222333334444444443333222222"
  , "111111000000001111111222222333334444444443333322222"
  , "111111100000011111111222222333334444444443333322222"
  , "111111111001111111111222222333334444444443333322222"
  , "111111111111111111111222222333334444444443333222222"
  , "111111111111111111112222222333333444444433333222222"
  , "111111111111111111112222222233333344444333333222222"
  , "111111111111111111122222222233333333333333332222222"
  , "111111111111111112222222222223333333333333332222222"
  , "111111111111122222222222222223333333333333322222222"
  , "111111111122222222222222222222233333333332222222221"
  , "111111122222222222222222222222222333333222222222221"
  , "111122222222222222222222222222222222222222222222211"
  , "111222222222222222222222222222222222222222222222111"
  , "112222222222222222222222222222222222222222222221111"
  , "122222222222333333222222222222222222222222221111111"
  , "122222222233333333332222222222222222222221111111111"
  , "222222223333333333333322222222222222221111111111111"
  , "222222233333333333333322222222222211111111111111111"
  , "222222233333333333333332222222221111111111111111111"
  , "222222333333444443333332222222211111111111111111111"
  , "222222333334444444333333222222211111111111111111111"
  , "222222333344444444433333222222111111111111111111111"
  , "222223333344444444433333222222111111111100111111111"
  , "222223333344444444433333222222111111110000001111111"
  , "222223333344444444433333222222111111100000000111111"
  , "222222333344444444433333222221111111000000000111111"
  , "122222333334444444333332222221111111000000000011111"
  , "122222333333444443333332222221111110000000000011111"
  , "122222233333333333333322222211111110000000000011111"
  , "112222223333333333333322222211111111000000000011111"
  , "112222222333333333333222222211111111000000000011111"
  , "111222222233333333322222222111111111100000000011111"
  , "111222222222233322222222222111111111111000000011111"
  , "111122222222222222222222222111111111111111111111111"
  , "111112222222222222222222221111111111111111111111111"
  , "111111122222222222222222211111111111111111111111111"
  , "111111111222222222222222111111111111111111111111111"
  , "111111111111222222222211111111111111111111111111111"
  ]

data AgentAgeSpan = Forever 
                  | Range Time Time 
                  deriving (Show, Eq)

data AgentDistribution = Scatter 
                       | Corner Discrete2dCoord 
                       | CombatCorners 
                       deriving (Show, Eq)

data Regrow = Immediate 
            | Rate Double 
            | Season Double Double Time
            deriving (Show, Eq)

data PolutionFormation = NoPolution 
                       | Polute Double Double 
                       deriving (Show, Eq)

data SugarScapeParams = SugarScapeParams 
  { sgParamsName           :: String

  , sgAgentCount           :: Int
  , sgAgentDistribution    :: AgentDistribution
  , spSugarRegrow          :: Regrow 
  , spSugarEndowmentRange  :: (Int, Int)
  , spSugarMetabolismRange :: (Int, Int)
  , spVisionRange          :: (Int, Int)
  , spReplaceAgents        :: Bool           -- replacement rule R_[a, b] on/off
  , spAgeSpan              :: AgentAgeSpan
  , spPolutionFormation    :: PolutionFormation
  , spPolutionDiffusion    :: Maybe Int
  
  -- Chapter III params
  , spSexRuleActive        :: Bool
  , spGenderRatio          :: Double        -- percentage of female agents in population
  , spFertStartRangeFemale :: (Int, Int)
  , spFertStartRangeMale   :: (Int, Int)
  , spFertEndRangeFemale   :: (Int, Int)
  , spFertEndRangeMale     :: (Int, Int)
  
  , spInheritance          :: Bool           -- inheritance rule I on / off
  
  , spCulturalProcess      :: Maybe Int      -- cultural process rule K on / off, with culture tag of given length

  , spCombat               :: Maybe Double   -- combat rule C_alpha on / off

  -- Chapter IV params
  , spSpiceEnabled         :: Bool           -- add spice to the landscape on/off
  , spSpiceRegrow          :: Regrow 
  , spSpiceEndowmentRange  :: (Int, Int)
  , spSpiceMetabolismRange :: (Int, Int)

  , spTradingEnabled       :: Bool            -- trading rule T on / off
  } 

instance Show SugarScapeParams where
  show params = 
    sgParamsName params ++

    "\n\nAgent count: \t\t\t" ++ show (sgAgentCount params) ++ 
    "\nAgent distribution: \t\t" ++ show (sgAgentDistribution params) ++

    "\n\nVision range: \t\t\t" ++ show (spVisionRange params) ++
    "\nAge age range: \t\t\t" ++ show (spAgeSpan params) ++

    "\n\nSugar regrow: \t\t\t" ++ show (spSugarRegrow params) ++ 
    "\nSugar endowment range: \t\t" ++ show (spSugarEndowmentRange params) ++
    "\nSugar metabolism range: \t" ++ show (spSugarMetabolismRange params) ++

    "\n\nSpice enabled: \t\t\t" ++ show (spSpiceEnabled params) ++
    "\nSpice regrow: \t\t\t" ++ show (spSpiceRegrow params) ++
    "\nSpice endowment range: \t\t" ++ show (spSpiceEndowmentRange params) ++
    "\nSpice metabolism range: \t" ++ show (spSpiceMetabolismRange params) ++

    "\n\nPolution formation: \t\t" ++ show (spPolutionFormation params) ++
    "\nPolution diffusion: \t\t" ++ show (spPolutionDiffusion params) ++

    "\n\nFemale gender ratio: \t\t" ++ show (spGenderRatio params) ++
    "\nFertility start-range female: \t" ++ show (spFertStartRangeFemale params) ++
    "\nFertility start-range male: \t" ++ show (spFertStartRangeMale params) ++
    "\nFertility end-range female: \t" ++ show (spFertEndRangeFemale params) ++
    "\nFertility send-range male: \t" ++ show (spFertEndRangeMale params) ++

    "\n\nR rule active (replace): \t" ++ show (spReplaceAgents params) ++
    "\nS rule active (mating): \t" ++ show (spSexRuleActive params) ++
    "\nI rule active (inheritance): \t" ++ show (spInheritance params) ++
    "\nK rule active (culture): \t" ++ show (spCulturalProcess params) ++
    "\nC rule active (combat): \t" ++ show (spCombat params) ++
    "\nT rule active (trading): \t" ++ show (spTradingEnabled params)

mkSugarScapeParams :: SugarScapeParams
mkSugarScapeParams = SugarScapeParams {
    sgParamsName           = "Default"
  , sgAgentCount           = 0
  , spSugarRegrow          = Immediate
  , sgAgentDistribution    = Scatter
  , spSugarEndowmentRange  = (0, 0)
  , spSugarMetabolismRange = (0, 0)
  , spVisionRange          = (0, 0)
  , spReplaceAgents        = False
  , spAgeSpan              = Forever
  , spPolutionFormation    = NoPolution
  , spPolutionDiffusion    = Nothing
  , spSexRuleActive        = False
  , spGenderRatio          = 0
  , spFertStartRangeFemale = (0, 0)
  , spFertStartRangeMale   = (0, 0)
  , spFertEndRangeFemale   = (0, 0)
  , spFertEndRangeMale     = (0, 0)
  , spInheritance          = False
  , spCulturalProcess      = Nothing
  , spCombat               = Nothing
  , spSpiceEnabled         = False
  , spSpiceRegrow          = Immediate
  , spSpiceEndowmentRange  = (0, 0)
  , spSpiceMetabolismRange = (0, 0)
  , spTradingEnabled       = False
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- Social Evolution with immediate regrow, page 27
mkParamsAnimationII_1 :: SugarScapeParams 
mkParamsAnimationII_1 = mkSugarScapeParams {
    sgParamsName           = "Animation II-1"
  , sgAgentCount           = 400     -- page 28
  , sgAgentDistribution    = Scatter
  , spSugarRegrow          = Immediate -- regrow to max immediately
  , spSugarEndowmentRange  = (5, 25) -- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
  , spSugarMetabolismRange = (1, 4)  -- NOTE: specified where? 1 - 4
  , spVisionRange          = (1, 6)  -- NOTE: set to 1-6 on page 24
  , spReplaceAgents        = False   -- no replacing of died agents
  , spAgeSpan              = Forever  -- agents dont die of age in this case
  }

-- Social Evolution with regrow rate of 1, page 29
mkParamsAnimationII_2 :: SugarScapeParams
mkParamsAnimationII_2 = mkParamsAnimationII_1 {
    sgParamsName  = "Animation II-2"
  , spSugarRegrow = Rate 1       -- regrow by 1 unit per step
  }

-- Wealth Distribution page 34
mkParamsAnimationII_3 :: SugarScapeParams
mkParamsAnimationII_3 = mkParamsAnimationII_2 {
    sgParamsName    = "Animation II-3"
  , sgAgentCount    = 250           -- page 33
  , spReplaceAgents = True          -- page 33
  , spAgeSpan       = Range 60 100  -- page 33
  }
-- wealth distribution as described on page 32-37
mkParamsAnimationII_4 :: SugarScapeParams
mkParamsAnimationII_4 = mkParamsAnimationII_3 {
    sgParamsName = "Animation II-4" -- same as G_1, M, R_60,100 => same as Animiation II-3
  }
   
mkParamsAnimationII_5 :: SugarScapeParams
mkParamsAnimationII_5 = mkParamsAnimationII_4 {
    sgParamsName = "Animation II-5" -- same as Animiation II-4
  }

-- Migration as described on page 42 and 43 in Animation II-6
mkParamsAnimationII_6 :: SugarScapeParams
mkParamsAnimationII_6 = mkParamsAnimationII_2 {
    sgParamsName        = "Animation II-6"
  , sgAgentCount        = 300              -- 300 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , sgAgentDistribution = Corner (20, 20)
  , spSugarRegrow       = Rate 0.5              -- 0.5 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , spVisionRange       = (1, 10)          -- increase vision to 10, see page 42, we suggest to to 15 to make the waves really prominent
  }

-- Seasonal Migration as described on page 44 and 45 in Animation II-7
mkParamsAnimationII_7 :: SugarScapeParams
mkParamsAnimationII_7 = mkParamsAnimationII_1 {
    sgParamsName  = "Animation II-7"
  , spSugarRegrow = Season 1 8 50 
  }

-- Polution as described on page 45 to 50 in Animation II-8
mkParamsAnimationII_8 :: SugarScapeParams
mkParamsAnimationII_8 = mkParamsAnimationII_2 {
    sgParamsName        = "Animation II-8"
  , spPolutionFormation = Polute 1 1
  , spPolutionDiffusion = Just 1
  }
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
-- page 57
mkParamsAnimationIII_1 :: SugarScapeParams
mkParamsAnimationIII_1 = mkParamsAnimationII_2 {
    sgParamsName           = "Animation III-1"
  , spSugarEndowmentRange  = (50, 100)
  , spReplaceAgents        = False          -- agents are NOT replaced...
  , spAgeSpan              = Range 60 100   -- ... but can die of age!
  , spSexRuleActive        = True           -- agents reproduce
  , spGenderRatio          = 0.5            -- equal ratio of gender
  , spFertStartRangeFemale = (12, 15)
  , spFertStartRangeMale   = (12, 15)
  , spFertEndRangeFemale   = (40, 50)
  , spFertEndRangeMale     = (50, 60)
  }

-- page 64, same as mkParamsAnimationIII_1 but with changed fertiliy ranges
mkParamsFigureIII_3 :: SugarScapeParams
mkParamsFigureIII_3 = mkParamsAnimationIII_1 {
    sgParamsName         = "Figure III-3"
  , spFertEndRangeFemale = (30, 40)
  , spFertEndRangeMale   = (40, 50)
  }

-- page 65, same as mkParamsAnimationIII_1 but with changed intiial endowment (=> lower requirements for child-bearing wealth)
mkParamsFigureIII_4 :: SugarScapeParams
mkParamsFigureIII_4 = mkParamsAnimationIII_1 {
    sgParamsName          = "Figure III-4"
  , spSugarEndowmentRange = (10, 40)
  }

-- Page 67, includes the inheritance rule
mkParamsFigureIII_7 :: SugarScapeParams
mkParamsFigureIII_7 = mkParamsAnimationIII_1 {
    sgParamsName  = "Figure III-7"
  , spInheritance = True  -- same as Animation III-1 but with inheritance on
  }

mkParamsAnimationIII_4 :: SugarScapeParams
mkParamsAnimationIII_4 = mkParamsFigureIII_7 {
    sgParamsName = "Animation III-4"
  }

-- cultural process, page 73
mkParamsAnimationIII_6 :: SugarScapeParams
mkParamsAnimationIII_6 = mkParamsAnimationII_2 {
    sgParamsName      = "Animation III-6"
  , spCulturalProcess = Just 10
  }

mkParamsAnimationIII_7 :: SugarScapeParams
mkParamsAnimationIII_7 = mkParamsAnimationIII_6 {
    sgParamsName = "Animation III-7"
  }

mkParamsFigureIII_8 :: SugarScapeParams
mkParamsFigureIII_8 = mkParamsAnimationIII_6 {
    sgParamsName = "Animation III-8"
  }

-- combat, page 82
mkParamsAnimationIII_9 :: SugarScapeParams
mkParamsAnimationIII_9 = mkParamsAnimationII_2 {
    sgParamsName        = "Animation III-9"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just (1 / 0)
  , spCulturalProcess   = Just 10 
  }

mkParamsAnimationIII_10 :: SugarScapeParams
mkParamsAnimationIII_10 = mkParamsAnimationIII_9 {
    sgParamsName = "Animation III-10"
  }

-- reward equal to a fixed value page 86 / 87
mkParamsAnimationIII_11 :: SugarScapeParams
mkParamsAnimationIII_11 = mkParamsAnimationII_2 {
    sgParamsName        = "Animation III-11"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just 2
  , spReplaceAgents     = True     
  , spAgeSpan           = Range 60 100 
  }

mkParamsAnimationIII_14 :: SugarScapeParams
mkParamsAnimationIII_14 = mkParamsAnimationII_2 {
    sgParamsName        = "Animation III-14"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just (1 / 0)
  , spCulturalProcess   = Just 10
  }

-- proto-history page 92/93
mkParamsAnimationIII_15 :: SugarScapeParams
mkParamsAnimationIII_15 = mkParamsAnimationIII_1 {
    sgParamsName      = "Animation III-15"
  , spCulturalProcess = Just 10
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER IV: Sugar and Spice: Trade comes to the sugarscape
------------------------------------------------------------------------------------------------------------------------
-- see page 99
mkParamsAnimationIV_1 :: SugarScapeParams
mkParamsAnimationIV_1 = mkParamsAnimationII_2 {
    sgParamsName           = "Animation IV-1"
  , spVisionRange          = (1, 10) -- book says 1-10 !!
  
  , spSugarMetabolismRange = (1, 5)  -- book says 1-5 which seems strange bcs maximum level on a site is 4 => agents with 5 will always die sooner or later
  , spSpiceMetabolismRange = (1, 5)   -- book says 1-5 which seems strange bcs maximum level on a site is 4 => agents with 5 will always die sooner or later
  , spSpiceEndowmentRange  = (5, 25)

  , spSpiceEnabled         = True
  , spSpiceRegrow          = Rate 1
  }

-- see page 108
mkParamsFigureIV_3 :: SugarScapeParams
mkParamsFigureIV_3 = mkParamsAnimationIV_1 {
    sgParamsName           = "Figure IV-3"
  , sgAgentCount           = 200     -- 200 only !
  , spVisionRange          = (1, 5)

  , spSugarMetabolismRange = (1, 5)  -- 1-5 !
  , spSpiceMetabolismRange = (1, 5)  -- 1-5 !

  , spSugarEndowmentRange  = (25, 50)
  , spSpiceEndowmentRange  = (25, 50)

  , spTradingEnabled = True
  }

mkParamsFigureIV_4 :: SugarScapeParams
mkParamsFigureIV_4 = mkParamsFigureIV_3 {
    sgParamsName = "Figure IV-4"
  }

mkParamsFigureIV_5 :: SugarScapeParams
mkParamsFigureIV_5 = mkParamsFigureIV_3 {
    sgParamsName = "Figure IV-5"
  }

-- see page 111 and 112
-- NOTE: we need to compare the carrying capacity with / without trade to one which has spice in it,
-- otherwise this will be not really comparable
mkCarryingCapacityWithSpice :: SugarScapeParams
mkCarryingCapacityWithSpice = mkParamsAnimationII_2 {
    sgParamsName           = "Carrying Capacity With Spice"
  , spSpiceMetabolismRange = (1, 4)     -- add spice 
  , spSpiceEndowmentRange  = (5, 25)    -- add spice
  , spSpiceEnabled         = True       -- enable spice
  , spSpiceRegrow          = Rate 1     -- enable regrow
  }

mkParamsFigureIV_6 :: SugarScapeParams
mkParamsFigureIV_6 = mkCarryingCapacityWithSpice {
    sgParamsName     = "Figure IV-6"
  , spTradingEnabled = True       -- enable trading
  }

-- see page 117 and 118
mkParamsFigureIV_7 :: SugarScapeParams
mkParamsFigureIV_7 = mkParamsFigureIV_3 {
    sgParamsName  = "Figure IV-7"
  , spVisionRange = (1, 1)
  }

mkParamsFigureIV_8 :: SugarScapeParams
mkParamsFigureIV_8 = mkParamsFigureIV_7 {
    sgParamsName = "Figure IV-8"
  }

mkParamsFigureIV_9 :: SugarScapeParams
mkParamsFigureIV_9 = mkParamsFigureIV_3 {
    sgParamsName  = "Figure IV-9"
  , spVisionRange = (1, 15)
  }