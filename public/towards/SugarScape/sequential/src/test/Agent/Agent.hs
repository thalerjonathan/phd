module Agent.Agent
  ( emptyEnvironment
  ) where

import Test.Tasty.QuickCheck as QC

import SugarScape.Agent.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model

instance Arbitrary SugAgentState where
  -- arbitrary :: Gen SugAgentState
  arbitrary = do
    randSugarMetab     <- choose (1, 4)
    randVision         <- choose (1, 6)
    randSugarEndowment <- choose (5, 25)
    randMaxAge         <- choose (60, 100)
    
    return SugAgentState {
      sugAgCoord        = (0, 0)
    , sugAgSugarMetab   = randSugarMetab
    , sugAgVision       = randVision
    , sugAgSugarLevel   = randSugarEndowment
    , sugAgMaxAge       = Just randMaxAge
    , sugAgAge          = 0
    , sugAgGender       = Male
    , sugAgFertAgeRange = (0, 0)
    , sugAgInitSugEndow = randSugarEndowment
    , sugAgChildren     = []
    , sugAgCultureTag   = []
    , sugAgTribe        = tagToTribe []
    , sugAgSpiceLevel   = randSugarEndowment
    , sugAgSpiceMetab   = randSugarMetab
    , sugAgInitSpiEndow = randSugarEndowment
    , sugAgBorrowed     = []
    , sugAgLent         = []
    , sugAgNetIncome    = 0
    , sugAgImmuneSystem = []
    , sugAgImSysGeno    = []
    , sugAgDiseases     = []
    }

emptyEnvironment :: SugEnvironment
emptyEnvironment = createDiscrete2d (0, 0) moore WrapBoth []
