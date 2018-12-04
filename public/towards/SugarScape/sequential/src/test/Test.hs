module Main where

import System.Random

import Test.Tasty

import Agent.Tests
import Discrete.Tests
--import Environment.Tests
--import Model.Tests

-- clear & stack test --test-arguments="--quickcheck-tests=1000 --quickcheck-replay="

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be reproducible
  --let g = mkStdGen seed
  --setStdGen g
  g <- newStdGen

  let sugarScapeTests 
        = testGroup "SugarScape Tests" 
            [ discreteTests
            , agentTests g
              -- envTests
              -- modelTests g
            ]

  defaultMain sugarScapeTests