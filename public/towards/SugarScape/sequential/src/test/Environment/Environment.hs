{-# LANGUAGE FlexibleInstances #-}
module Environment.Environment
  ( prop_env_regrow_rate
  , prop_env_regrow_rate_full
  , prop_env_regrow_full
  ) where

import Test.Tasty.QuickCheck as QC

import SugarScape.Core.Discrete
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import Utils.Runner

instance Arbitrary SugEnvSite where
  -- arbitrary :: Gen SugEnvSite
  arbitrary = do
    cap <- choose (0, 4)
    lvl <- choose (0, cap)

    return SugEnvSite {
      sugEnvSiteSugarCapacity = cap
    , sugEnvSiteSugarLevel    = lvl

    , sugEnvSiteSpiceLevel    = lvl
    , sugEnvSiteSpiceCapacity = cap

    , sugEnvSiteOccupier      = Nothing
    , sugEnvSitePolutionLevel = 0
    }

instance Arbitrary (Discrete2d SugEnvSite) where
  -- arbitrary :: Gen (Discrete2d SugEnvSite)
  arbitrary = do
    --dimX <- choose (1, 100)
    --dimY <- choose (1, 100)
  
    let dimX = 100
        dimY = 100

        dim = (dimX, dimY)
        n   = moore
        w   = WrapBoth

    cs <- vector (dimX * dimY)

    let coords  = [(x, y) | x <- [0..dimX - 1], y <- [0..dimY - 1]]
        csCoord = zip coords cs

    return $ createDiscrete2d dim n w csCoord

prop_env_regrow_rate :: Positive Double
                     -> Discrete2d SugEnvSite
                     -> Bool
prop_env_regrow_rate (Positive rate) env0 
    = all posSugarLevel cs'    &&
      all levelLTESugarMax cs'
  where
    params = mkSugarScapeScenario { spSugarRegrow = Rate rate }
    env'   = sugEnvBehaviour params 0 env0
    cs'    = allCells env'

-- test that after max level / rate steps the difference between level and capacity in all cells is 0
prop_env_regrow_rate_full :: Positive Double
                          -> Discrete2d SugEnvSite
                          -> Bool
prop_env_regrow_rate_full (Positive rate) env0 
    = all posSugarLevel cs' && all fullSugarLevel cs'
  where
    params = mkSugarScapeScenario { spSugarRegrow = Rate rate }
    steps  = ceiling ((fromIntegral maxSugarCapacitySite / rate) :: Double)
    env'   = runSugEnvSteps steps 0 env0 (sugEnvBehaviour params)
    cs'    = allCells env'

-- test growback after 1 step
-- test that after 1 step the difference between level and capacity on all sites is 0
prop_env_regrow_full :: Discrete2d SugEnvSite
                     -> Bool
prop_env_regrow_full env0 
    = all fullSugarLevel cs && all posSugarLevel cs 
  where
    params = mkSugarScapeScenario { spSugarRegrow = Immediate }
    env'   = sugEnvBehaviour params 0 env0
    cs     = allCells env'

-- TODO: implement season property-tests

fullSugarLevel :: SugEnvSite -> Bool
fullSugarLevel c = sugEnvSiteSugarLevel c == sugEnvSiteSugarCapacity c

posSugarLevel :: SugEnvSite -> Bool
posSugarLevel c = sugEnvSiteSugarLevel c > 0

levelLTESugarMax :: SugEnvSite -> Bool
levelLTESugarMax c = sugEnvSiteSugarLevel c <= sugEnvSiteSugarCapacity c

