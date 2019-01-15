module Model.Tests 
  ( modelTests
  ) where

import Control.Monad.Random
import Test.Tasty
import Test.Tasty.HUnit as Unit

import Model.Model

repls :: Int
repls = 5

confidence :: Double
confidence = 0.95

modelTests :: RandomGen g 
           => g
           -> TestTree 
modelTests g 
  = testGroup "Model Tests" 
      [ Unit.testCase "Cultural Dynamics" $ prop_culture_dynamics g repls confidence
      , Unit.testCase "Disease Dynamics All Recover" $ prop_disease_dynamics_allrecover g repls confidence
      , Unit.testCase "Disease Dynamics Minority Recover" $ prop_disease_dynamics_minorityrecover g repls confidence
      , Unit.testCase "Inheritance Gini" $ prop_inheritance_gini g repls confidence
      , Unit.testCase "Trading Dynamics" $ prop_trading_dynamics g repls confidence
      , Unit.testCase "Terracing" $ prop_terracing g repls confidence
      , Unit.testCase "Carrying Capacity" $ prop_carrying_cap g repls confidence
      , Unit.testCase "Wealth Distribution" $ prop_wealth_dist g repls confidence
      ]