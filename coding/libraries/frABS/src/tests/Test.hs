module Test where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Environment.Discrete

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "FrABS Tests" [unitTests]

unitTests = 
  testGroup 
    "FrABS Unit tests"
      [test_environment_discrete_group]
