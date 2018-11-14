module SugarScape.Core.Scenario
  ( AgentAgeSpan (..)

  , SugarScapeScenario (..)
  , AgentDistribution (..)
  , Regrow (..)
  , PolutionFormation (..)

  , sugarScapeScenarios

  , mkSugarScapeScenario

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
  , mkParamsFigureIV_10
  , mkParamsFigureIV_11
  , mkParamsFigureIV_14
  ) where

import SugarScape.Core.Common
import SugarScape.Core.Discrete

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

data SugarScapeScenario = SugarScapeScenario 
  { sgScenarioName           :: String

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
  
  , spCreditEnabled        :: Maybe (Double, Double)    -- credit rule I on / off
  } 

instance Show SugarScapeScenario where
  show params = 
    sgScenarioName params ++

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
    "\nT rule active (trading): \t" ++ show (spTradingEnabled params) ++
    "\nL rule active (credits): \t" ++ show (spTradingEnabled params)

sugarScapeScenarios :: [SugarScapeScenario]
sugarScapeScenarios = [
    mkParamsAnimationII_1
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
  , mkParamsFigureIV_10
  , mkParamsFigureIV_11
  , mkParamsFigureIV_14
  ]

mkSugarScapeScenario :: SugarScapeScenario
mkSugarScapeScenario = SugarScapeScenario {
    sgScenarioName           = "Default"
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
  , spCreditEnabled        = Nothing
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- Social Evolution with immediate regrow, page 27
mkParamsAnimationII_1 :: SugarScapeScenario 
mkParamsAnimationII_1 = mkSugarScapeScenario {
    sgScenarioName           = "Animation II-1"
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
mkParamsAnimationII_2 :: SugarScapeScenario
mkParamsAnimationII_2 = mkParamsAnimationII_1 {
    sgScenarioName  = "Animation II-2"
  , spSugarRegrow = Rate 1       -- regrow by 1 unit per step
  }

-- Wealth Distribution page 34
mkParamsAnimationII_3 :: SugarScapeScenario
mkParamsAnimationII_3 = mkParamsAnimationII_2 {
    sgScenarioName    = "Animation II-3"
  , sgAgentCount    = 250           -- page 33
  , spReplaceAgents = True          -- page 33
  , spAgeSpan       = Range 60 100  -- page 33
  }
-- wealth distribution as described on page 32-37
mkParamsAnimationII_4 :: SugarScapeScenario
mkParamsAnimationII_4 = mkParamsAnimationII_3 {
    sgScenarioName = "Animation II-4" -- same as G_1, M, R_60,100 => same as Animiation II-3
  }
   
mkParamsAnimationII_5 :: SugarScapeScenario
mkParamsAnimationII_5 = mkParamsAnimationII_4 {
    sgScenarioName = "Animation II-5" -- same as Animiation II-4
  }

-- Migration as described on page 42 and 43 in Animation II-6
mkParamsAnimationII_6 :: SugarScapeScenario
mkParamsAnimationII_6 = mkParamsAnimationII_2 {
    sgScenarioName        = "Animation II-6"
  , sgAgentCount        = 300              -- 300 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , sgAgentDistribution = Corner (20, 20)
  , spSugarRegrow       = Rate 0.5              -- 0.5 otherwise no waves, see https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
  , spVisionRange       = (1, 10)          -- increase vision to 10, see page 42, we suggest to to 15 to make the waves really prominent
  }

-- Seasonal Migration as described on page 44 and 45 in Animation II-7
mkParamsAnimationII_7 :: SugarScapeScenario
mkParamsAnimationII_7 = mkParamsAnimationII_1 {
    sgScenarioName  = "Animation II-7"
  , spSugarRegrow = Season 1 8 50 
  }

-- Polution as described on page 45 to 50 in Animation II-8
mkParamsAnimationII_8 :: SugarScapeScenario
mkParamsAnimationII_8 = mkParamsAnimationII_2 {
    sgScenarioName        = "Animation II-8"
  , spPolutionFormation = Polute 1 1
  , spPolutionDiffusion = Just 1
  }
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
-- page 57
mkParamsAnimationIII_1 :: SugarScapeScenario
mkParamsAnimationIII_1 = mkParamsAnimationII_2 {
    sgScenarioName           = "Animation III-1"
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
mkParamsFigureIII_3 :: SugarScapeScenario
mkParamsFigureIII_3 = mkParamsAnimationIII_1 {
    sgScenarioName         = "Figure III-3"
  , spFertEndRangeFemale = (30, 40)
  , spFertEndRangeMale   = (40, 50)
  }

-- page 65, same as mkParamsAnimationIII_1 but with changed intiial endowment (=> lower requirements for child-bearing wealth)
mkParamsFigureIII_4 :: SugarScapeScenario
mkParamsFigureIII_4 = mkParamsAnimationIII_1 {
    sgScenarioName          = "Figure III-4"
  , spSugarEndowmentRange = (10, 40)
  }

-- Page 67, includes the inheritance rule
mkParamsFigureIII_7 :: SugarScapeScenario
mkParamsFigureIII_7 = mkParamsAnimationIII_1 {
    sgScenarioName  = "Figure III-7"
  , spInheritance = True  -- same as Animation III-1 but with inheritance on
  }

mkParamsAnimationIII_4 :: SugarScapeScenario
mkParamsAnimationIII_4 = mkParamsFigureIII_7 {
    sgScenarioName = "Animation III-4"
  }

-- cultural process, page 73
mkParamsAnimationIII_6 :: SugarScapeScenario
mkParamsAnimationIII_6 = mkParamsAnimationII_2 {
    sgScenarioName      = "Animation III-6"
  , spCulturalProcess = Just 10
  }

mkParamsAnimationIII_7 :: SugarScapeScenario
mkParamsAnimationIII_7 = mkParamsAnimationIII_6 {
    sgScenarioName = "Animation III-7"
  }

mkParamsFigureIII_8 :: SugarScapeScenario
mkParamsFigureIII_8 = mkParamsAnimationIII_6 {
    sgScenarioName = "Animation III-8"
  }

-- combat, page 82
mkParamsAnimationIII_9 :: SugarScapeScenario
mkParamsAnimationIII_9 = mkParamsAnimationII_2 {
    sgScenarioName        = "Animation III-9"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just (1 / 0)
  , spCulturalProcess   = Just 10 
  }

mkParamsAnimationIII_10 :: SugarScapeScenario
mkParamsAnimationIII_10 = mkParamsAnimationIII_9 {
    sgScenarioName = "Animation III-10"
  }

-- reward equal to a fixed value page 86 / 87
mkParamsAnimationIII_11 :: SugarScapeScenario
mkParamsAnimationIII_11 = mkParamsAnimationII_2 {
    sgScenarioName        = "Animation III-11"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just 2
  , spReplaceAgents     = True     
  , spAgeSpan           = Range 60 100 
  }

mkParamsAnimationIII_14 :: SugarScapeScenario
mkParamsAnimationIII_14 = mkParamsAnimationII_2 {
    sgScenarioName        = "Animation III-14"
  , sgAgentDistribution = CombatCorners
  , spCombat            = Just (1 / 0)
  , spCulturalProcess   = Just 10
  }

-- proto-history page 92/93
mkParamsAnimationIII_15 :: SugarScapeScenario
mkParamsAnimationIII_15 = mkParamsAnimationIII_1 {
    sgScenarioName      = "Animation III-15"
  , spCulturalProcess = Just 10
  }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER IV: Sugar and Spice: Trade comes to the sugarscape
------------------------------------------------------------------------------------------------------------------------
-- see page 99
mkParamsAnimationIV_1 :: SugarScapeScenario
mkParamsAnimationIV_1 = mkParamsAnimationII_2 {
    sgScenarioName           = "Animation IV-1"
  , spVisionRange          = (1, 10) -- book says 1-10 !!
  
  , spSugarMetabolismRange = (1, 5)  -- book says 1-5 which seems strange bcs maximum level on a site is 4 => agents with 5 will always die sooner or later
  , spSpiceMetabolismRange = (1, 5)   -- book says 1-5 which seems strange bcs maximum level on a site is 4 => agents with 5 will always die sooner or later
  , spSpiceEndowmentRange  = (5, 25)

  , spSpiceEnabled         = True
  , spSpiceRegrow          = Rate 1
  }

-- see page 108
mkParamsFigureIV_3 :: SugarScapeScenario
mkParamsFigureIV_3 = mkParamsAnimationIV_1 {
    sgScenarioName           = "Figure IV-3"
  , sgAgentCount           = 200     -- 200 only !
  , spVisionRange          = (1, 5)

  , spSugarMetabolismRange = (1, 5)  -- 1-5 !
  , spSpiceMetabolismRange = (1, 5)  -- 1-5 !

  , spSugarEndowmentRange  = (25, 50)
  , spSpiceEndowmentRange  = (25, 50)

  , spTradingEnabled = True
  }

mkParamsFigureIV_4 :: SugarScapeScenario
mkParamsFigureIV_4 = mkParamsFigureIV_3 {
    sgScenarioName = "Figure IV-4"
  }

mkParamsFigureIV_5 :: SugarScapeScenario
mkParamsFigureIV_5 = mkParamsFigureIV_3 {
    sgScenarioName = "Figure IV-5"
  }

-- see page 111 and 112
-- NOTE: we need to compare the carrying capacity with / without trade to one which has spice in it,
-- otherwise this will be not really comparable
mkCarryingCapacityWithSpice :: SugarScapeScenario
mkCarryingCapacityWithSpice = mkParamsAnimationII_2 {
    sgScenarioName           = "Carrying Capacity With Spice"
  , spSpiceMetabolismRange = (1, 4)     -- add spice 
  , spSpiceEndowmentRange  = (5, 25)    -- add spice
  , spSpiceEnabled         = True       -- enable spice
  , spSpiceRegrow          = Rate 1     -- enable regrow
  }

mkParamsFigureIV_6 :: SugarScapeScenario
mkParamsFigureIV_6 = mkCarryingCapacityWithSpice {
    sgScenarioName     = "Figure IV-6"
  , spTradingEnabled = True       -- enable trading
  }

-- see page 117 and 118
mkParamsFigureIV_7 :: SugarScapeScenario
mkParamsFigureIV_7 = mkParamsFigureIV_3 {
    sgScenarioName  = "Figure IV-7"
  , spVisionRange = (1, 1)
  }

mkParamsFigureIV_8 :: SugarScapeScenario
mkParamsFigureIV_8 = mkParamsFigureIV_7 {
    sgScenarioName = "Figure IV-8"
  }

mkParamsFigureIV_9 :: SugarScapeScenario
mkParamsFigureIV_9 = mkParamsFigureIV_3 {
    sgScenarioName  = "Figure IV-9"
  , spVisionRange = (1, 15)
  }

-- see page 121
mkParamsFigureIV_10 :: SugarScapeScenario
mkParamsFigureIV_10 = mkParamsFigureIV_3 {
    sgScenarioName    = "Figure IV-10"
  , spReplaceAgents = True          -- R rule is turned on
  , spAgeSpan       = Range 60 100  -- and agents can die of age
  }

mkParamsFigureIV_11 :: SugarScapeScenario
mkParamsFigureIV_11 = mkParamsFigureIV_10 {
    sgScenarioName = "Figure IV-10"
  }

-- see page 123
mkParamsFigureIV_14 :: SugarScapeScenario
mkParamsFigureIV_14 = mkParamsFigureIV_3 {
    sgScenarioName           = "Figure IV-14"
  , sgAgentCount           = 400
  , spAgeSpan              = Range 60 100

  , spVisionRange          = (1, 6)

  , spSugarMetabolismRange = (1, 4)  -- 1-5 !
  , spSpiceMetabolismRange = (1, 4)  -- 1-5 !

  , spSugarEndowmentRange  = (5, 25)        -- not specified in book but 25-50 leads to extinction because agents not engaing in mating because cant gather enough wealth
  , spSpiceEndowmentRange  = (5, 25)        -- not specified in book but 25-50 leads to extinction because agents not engaing in mating because cant gather enough wealth

  , spFertStartRangeFemale = (12, 15)
  , spFertStartRangeMale   = (12, 15)
  , spFertEndRangeFemale   = (35, 45)
  , spFertEndRangeMale     = (45, 55)
  , spSexRuleActive        = True
  , spGenderRatio          = 0.5
  }

-- NOTE: we didn't implement
--  Effect of Culturally Varying Preferences, page 124 - 126
--  Externalities and Price Disequilibrium: The effect of Pollution, page 126 - 118
--  On The Evolution of Foresight page 129 / 130