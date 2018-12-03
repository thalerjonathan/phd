module Model.Tests 
  ( modelTests
  ) where

import Control.Monad.Random
import Test.Tasty
import Test.Tasty.HUnit as Unit

import Model.Model

modelTests :: RandomGen g 
           => g
           -> TestTree 
modelTests g 
  = testGroup "Model Tests" 
      [ Unit.testCase "Disease Dynamics All Recover" $ prop_disease_dynamics_allrecover g
      , Unit.testCase "Disease Dynamics Minority Recover" $ prop_disease_dynamics_minorityrecover g
      , Unit.testCase "Trading Dynamics" $ prop_trading_dynamics g
      , Unit.testCase "Cultural Dynamics" $ prop_culture_dynamics g
      , Unit.testCase "Inheritance Gini" $ prop_inheritance_gini g
      , Unit.testCase "Terracing" $ prop_terracing g
      , Unit.testCase "Carrying Capacity" $ prop_carrying_cap g
      , Unit.testCase "Wealth Distribution" $ prop_wealth_dist g
      ]