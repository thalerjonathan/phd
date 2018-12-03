module Environment.Tests
  ( envTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Environment.Environment

envTests :: TestTree 
envTests = testGroup "Environment Tests"
            [ QC.testProperty "Regrow By Rate" prop_env_regrow_rate
            , QC.testProperty "Regrow By Max" prop_env_regrow_full
            , QC.testProperty "Regrow To Full" prop_env_regrow_rate_full ]