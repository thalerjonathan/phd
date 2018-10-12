module Main where

import System.Random

import Test.Tasty
--import Test.Tasty.QuickCheck as QC

--import SugarScape.Discrete 
--import SugarScape.Model

import Agent
import Environment
import Simulation

-- clear & stack test --test-arguments="--quickcheck-tests=100 --quickcheck-replay="

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g = mkStdGen seed
  setStdGen g

{-
  putStrLn ""
  x <- generate arbitrary :: IO (Discrete2d SugEnvCell)
  print x
-}

  let sugarScapeTests = testGroup "SugarScape Tests" 
                              [ 
                                agentTests g 
                              , simTests g
                              , envTests g 
                              ]

  defaultMain sugarScapeTests