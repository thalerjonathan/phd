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
  _g <- newStdGen

  -- NOTE: only modelTests should receive RNG! Reason: they are unit-tests and
  -- thus need a RNG seed. In other unit-test cases there is no need for a RNG
  -- because completely deterministic, or a fake RNG is used (using mkStdGen 42)
  -- when agent computation never makes use of it,
  let sugarScapeTests 
        = testGroup "SugarScape Tests" 
            [ discreteTests
            , agentTests
              -- envTests
              -- modelTests g
            ]

  defaultMain sugarScapeTests