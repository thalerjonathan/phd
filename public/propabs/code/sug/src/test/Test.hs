module Main where

import System.Random

import Test.Tasty

import Environment
import Simulation

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g = mkStdGen seed
  setStdGen g

  let sugarScapeTests = testGroup "SugarScape Tests" [ envTests, simTests ]
  
  defaultMain sugarScapeTests
