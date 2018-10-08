module Main where

import System.Random

import Test.Tasty

import SIRYampa
import SIRYampaTests
import StatsUtils

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g = mkStdGen seed
  setStdGen g

  --print $ std [10,2,38,23,38,23,21]

  --print $ tTest "test" [1..10] 6 0.05

  --let t = tTest [1, 1, 1, 1] 5 0.05
  --print t 

  print $ prop_SIRSim g [Infected] --[Infected,Recovered,Recovered]

  defaultMain $ sirYampaTests g