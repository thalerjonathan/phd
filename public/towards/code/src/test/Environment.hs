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

import Runner

import Debug.Trace


instance Arbitrary SugEnvCell where
  -- arbitrary :: Gen SugEnvCell
  arbitrary = do
    cap <- choose (0, 4)
    lvl <- choose (0, cap)

    return SugEnvCell {
      sugEnvSugarCapacity = cap
    , sugEnvSugarLevel    = lvl
    , sugEnvOccupier      = Nothing
    }

instance Arbitrary (Discrete2d SugEnvCell) where
  -- arbitrary :: Gen (Discrete2d SugEnvCell)
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
                     -> Discrete2d SugEnvCell
                     -> Bool
prop_env_regrow_rate g (Positive rate) env0 
    = all posSugarLevel cs'    &&
      all levelLTESugarMax cs'
  where
    sugParams          = mkSugarScapeParams { spSugarGrowBackRate = rate }
    (_, env', _, _, _) = runAgentSF (sugEnvironment sugParams) defaultAbsState env0 g
    cs'                = allCells env'

-- test that after max level / rate steps the difference between level and capacity in all cells is 0
prop_env_regrow_rate_full :: RandomGen g 
                          => g
                          -> Positive Double
                          -> Discrete2d SugEnvCell
                          -> Bool
prop_env_regrow_rate_full g (Positive rate) env0 
    = all posSugarLevel cs' && all fullSugarLevel cs'
  where
    sugParams       = mkSugarScapeParams { spSugarGrowBackRate = rate }
    steps           = ceiling ((fromIntegral maxSugarCapacityCell / rate) :: Double)
    (outs, _, _, _) = runAgentSFSteps steps (sugEnvironment sugParams) defaultAbsState env0 g
    (_, env')       = last outs
    cs'             = allCells env'

-- test growback after 1 step
-- test that after 1 step the difference between level and capacity in all cells is 0
prop_env_regrow_full :: RandomGen g 
                     => g
                     -> Discrete2d SugEnvCell
                     -> Bool
prop_env_regrow_full g env0 
    = all fullSugarLevel cs && all posSugarLevel cs 
  where
    -- with a regrow-rate < 0 the sugar regrows to max within 1 step
    sugParams          = mkSugarScapeParams { spSugarGrowBackRate = -1 }
    (_, env', _, _, _) = runAgentSF (sugEnvironment sugParams) defaultAbsState env0 g
    cs                 = allCells env'

fullSugarLevel :: SugEnvCell -> Bool
fullSugarLevel c = sugEnvSugarLevel c == sugEnvSugarCapacity c

posSugarLevel :: SugEnvCell -> Bool
posSugarLevel c = sugEnvSugarLevel c > 0

levelLTESugarMax :: SugEnvCell -> Bool
levelLTESugarMax c = sugEnvSugarLevel c <= sugEnvSugarCapacity c