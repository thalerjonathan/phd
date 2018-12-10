{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module Agent.Agent
  ( arbitraryAgentStateFromScenario
  ) where

import Test.Tasty.QuickCheck as QC

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Core.Model
import SugarScape.Core.Scenario

-- NOTE: problem is that it contains AgentDef which in turn contains MSFs, which 
-- can not be checked for equality (at least not in Haskell), ignoring MSF
instance Eq (SugAgentOut g) where
  (==) ao0 ao1 = aoKill ao0       == aoKill ao1 &&
                 aoCreate ao0     == aoCreate ao1 &&
                 aoEvents ao0     == aoEvents ao1

-- NOTE: AgentDef contains MSF, which 
-- can not be checked for equality (at least not in Haskell), ignoring MSF
instance Eq (SugAgentDef g) where
  (==) ad0 ad1 = adId ad0      == adId ad1 && 
                 adInitObs ad0 == adInitObs ad1 


-- NOTE: this instance creates default random SugAgentState. Some fields might
-- be overriden by different random values in some property-tests.
instance Arbitrary SugAgentState where
  -- arbitrary :: Gen SugAgentState
  arbitrary = arbitraryAgentStateFromScenario mkParamsAnimationII_3

arbitraryAgentStateFromScenario :: SugarScapeScenario -> Gen SugAgentState
arbitraryAgentStateFromScenario sc = do
  randSugarMetab     <- choose $ spSugarMetabolismRange sc
  randVision         <- choose $ spVisionRange sc
  randSugarEndowment <- choose $ spSugarEndowmentRange sc
  ageSpan            <- randomAgentAge $ spAgeSpan sc
  randGender         <- randomGender $ spGenderRatio sc
  randFertAgeRange   <- randomFertilityRange sc randGender
  randCultureTag     <- randomCultureTag sc
  randSpiceEndowment <- choose $ spSpiceEndowmentRange sc
  randSpiceMetab     <- choose $ spSpiceMetabolismRange sc
  randImmuneSystem   <- randomImmuneSystem sc
  randDiseases       <- randomDiseases sc

  let initSugar = fromIntegral randSugarEndowment
      initSpice = fromIntegral randSpiceEndowment

  return SugAgentState {
    sugAgCoord        = (0, 0)  -- default position
  , sugAgSugarMetab   = randSugarMetab
  , sugAgVision       = randVision
  , sugAgSugarLevel   = initSugar
  , sugAgMaxAge       = ageSpan
  , sugAgAge          = 0
  , sugAgGender       = randGender
  , sugAgFertAgeRange = randFertAgeRange
  , sugAgInitSugEndow = initSugar
  , sugAgChildren     = []
  , sugAgCultureTag   = randCultureTag
  , sugAgTribe        = tagToTribe randCultureTag
  , sugAgSpiceLevel   = initSpice
  , sugAgInitSpiEndow = initSpice
  , sugAgSpiceMetab   = randSpiceMetab
  , sugAgTrades       = []
  , sugAgBorrowed     = []
  , sugAgLent         = []
  , sugAgNetIncome    = 0
  , sugAgImmuneSystem = randImmuneSystem
  , sugAgImSysGeno    = randImmuneSystem
  , sugAgDiseases     = randDiseases
  }

randomDiseases :: SugarScapeScenario -> Gen [Disease]
randomDiseases sc = 
  case spDiseasesEnabled sc of 
    Nothing                       -> return []
    Just (_, _, _, n, masterList) -> 
      mapM (const $ elements masterList) [1..n]

randomImmuneSystem :: SugarScapeScenario -> Gen ImmuneSystem
randomImmuneSystem sc = 
  case spDiseasesEnabled sc of 
    Nothing              -> return []
    Just (n, _, _, _, _) -> vector n 

randomCultureTag :: SugarScapeScenario -> Gen CultureTag
randomCultureTag sc = 
  case spCulturalProcess sc of 
    Nothing -> return []
    Just n  -> vector n

randomGender :: Double -> Gen AgentGender
randomGender p = do
  r <- choose ((0, 1) :: (Double, Double))
  if r >= p
    then return Male
    else return Female

randomFertilityRange :: SugarScapeScenario 
                     -> AgentGender
                     -> Gen (Int, Int)
randomFertilityRange sc Male = do
  from <- choose $ spFertStartRangeMale sc
  to   <- choose $ spFertEndRangeMale sc
  return (from, to)
randomFertilityRange sc Female = do
  from <- choose $ spFertStartRangeFemale sc
  to   <- choose $ spFertEndRangeFemale sc
  return (from, to)

randomAgentAge :: AgentAgeSpan -> Gen (Maybe Int)
randomAgentAge Forever         = return Nothing
randomAgentAge (Range from to) = do
  randMaxAge <- choose (from, to)
  return $ Just randMaxAge
