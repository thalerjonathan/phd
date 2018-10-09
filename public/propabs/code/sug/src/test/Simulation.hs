module Simulation 
  ( simTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

simTests :: TestTree 
simTests = testGroup "Simulation Tests" [ QC.testProperty "Carrying Capacity" prop_carrying_cap ]

prop_carrying_cap :: Bool
prop_carrying_cap = True -- TODO: implement
