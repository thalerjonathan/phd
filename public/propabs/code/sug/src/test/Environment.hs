module Environment
  ( envTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

{-
import SugarScape.Model

instance Arbitrary SugEnvCell where
  -- arbitrary :: Gen SugEnvCell
  arbitrary = SugEnvCell {
    sugEnvSugarCapacity = 0
  , sugEnvSugarLevel    = 0
  , sugEnvOccupier      = occ
  }
-}

envTests :: TestTree 
envTests = testGroup "Environment Tests"
            [ QC.testProperty "Regrow Rate" prop_env_regrow_rate 
            , QC.testProperty "Regrow Rate" prop_env_regrow_full ]

prop_env_regrow_rate :: Bool
prop_env_regrow_rate = True

prop_env_regrow_full :: Bool
prop_env_regrow_full = True