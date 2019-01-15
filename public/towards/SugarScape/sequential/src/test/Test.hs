module Main where

import System.Random

import Test.Tasty

import Agent.Tests
import Discrete.Tests
import Environment.Tests
import Model.Tests
-- import Utils.StatsUtils

-- clear & stack test --test-arguments="--quickcheck-tests=1000 --quickcheck-replay="

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be reproducible
  let g = mkStdGen seed
  setStdGen g
  --g <- newStdGen

  -- let xs  = [0.0266, 0.0305, 0.0222, 0.0321, 0.0406, 0.0024, 0.0208, 0.0072, 0.0706, 0.0281]
  --    xs' = [0.02, 0.03, 0.02, 0.04, 0.04, 0.01, 0.02, 0.02, 0.01, 0.02]
  -- print $ tTest "t-test" xs 0.05 0.05 LT

  -- NOTE: only modelTests should receive RNG! Reason: they are unit-tests and
  -- thus need a RNG seed. In other unit-test cases there is no need for a RNG
  -- because completely deterministic, or a fake RNG is used (using mkStdGen 42)
  -- when agent computation never makes use of it,
  let sugarScapeTests 
        = testGroup "SugarScape Tests" 
            [ modelTests g
            , discreteTests
            , agentTests
            , envTests
            ]

  defaultMain sugarScapeTests