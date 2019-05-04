module Main where

import Test.Tasty

import Agent.Tests
import Discrete.Tests
import Environment.Tests

-- clear & stack test --test-arguments="--quickcheck-tests=1000 --quickcheck-replay="

main :: IO ()
main = do
  let sugarScapeTests 
        = testGroup "SugarScape Tests" 
            [ discreteTests
            , agentTests
            , envTests
            ]
  
  defaultMain sugarScapeTests