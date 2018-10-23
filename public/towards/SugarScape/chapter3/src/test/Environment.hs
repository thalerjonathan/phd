{-# LANGUAGE FlexibleInstances #-}
module Environment
  ( envTests
  ) where

import Control.Monad.Random
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Environment
import SugarScape.Model
import SugarScape.Simulation

import Runner

import Debug.Trace

instance Arbitrary SugEnvSite where
  -- arbitrary :: Gen SugEnvSite
  arbitrary = do
    cap <- choose (0, 4)
    lvl <- choose (0, cap)

    return SugEnvSite {
      sugEnvSiteSugarCapacity = cap
    , sugEnvSiteSugarLevel    = lvl
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

envTests :: RandomGen g 
         => g
         -> TestTree 
envTests g = testGroup "Environment Tests"
            [ QC.testProperty "Regrow By Rate" $ prop_env_regrow_rate g
            , QC.testProperty "Regrow By Max" $ prop_env_regrow_full g
            , QC.testProperty "Regrow To Full" $ prop_env_regrow_rate_full g  ]

prop_env_regrow_rate :: RandomGen g 
                     => g
                     -> Positive Double
                     -> Discrete2d SugEnvSite
                     -> Bool
prop_env_regrow_rate g (Positive rate) env0 
    = all posSugarLevel cs'    &&
      all levelLTESugarMax cs'
  where
    params    = mkSugarScapeParams { spSugarRegrow = Rate rate }
    (env', _) = runEnvSF env0 (sugEnvironmentSf params)
    cs'       = allCells env'

-- test that after max level / rate steps the difference between level and capacity in all cells is 0
prop_env_regrow_rate_full :: RandomGen g 
                          => g
                          -> Positive Double
                          -> Discrete2d SugEnvSite
                          -> Bool
prop_env_regrow_rate_full g (Positive rate) env0 
    = all posSugarLevel cs' && all fullSugarLevel cs'
  where
    params    = mkSugarScapeParams { spSugarRegrow = Rate rate }
    steps     = ceiling ((fromIntegral maxSugarCapacitySite / rate) :: Double)
    (env', _) = runSugEnvSteps steps env0 (sugEnvironmentSf params)
    cs'       = allCells env'

-- test growback after 1 step
-- test that after 1 step the difference between level and capacity on all sites is 0
prop_env_regrow_full :: RandomGen g 
                     => g
                     -> Discrete2d SugEnvSite
                     -> Bool
prop_env_regrow_full g env0 
    = all fullSugarLevel cs && all posSugarLevel cs 
  where
    params    = mkSugarScapeParams { spSugarRegrow = Immediate }
    (env', _) = runEnvSF env0 (sugEnvironmentSf params)
    cs        = allCells env'

fullSugarLevel :: SugEnvSite -> Bool
fullSugarLevel c = sugEnvSiteSugarLevel c == sugEnvSiteSugarCapacity c

posSugarLevel :: SugEnvSite -> Bool
posSugarLevel c = sugEnvSiteSugarLevel c > 0

levelLTESugarMax :: SugEnvSite -> Bool
levelLTESugarMax c = sugEnvSiteSugarLevel c <= sugEnvSiteSugarCapacity c