module Main where

import Test.Tasty

import Agent.Tests
import Discrete.Tests
import Environment.Tests

-- clear & stack test --test-arguments="--quickcheck-tests=1000 --quickcheck-replay="

seed :: Int
seed = 42

main :: IO ()
main = do
  let sugarScapeTests 
        = testGroup "SugarScape Tests" 
            [ discreteTests
            , agentTests
            , envTests
            ]
  
  defaultMain sugarScapeTests