module Main where

import System.Random

import Test.Tasty

import SIR
import SIRSDYampa
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

  let ctx     = defaultSIRCtx g
      absDyns = runSIRYampa ctx
  writeAggregatesToFile "yampaABS.m" 0.01 absDyns

  let sdDyns = runYampaSD 999 1 0 5 0.05 15 200 0.01
  writeAggregatesToFile "yampaSD.m" 0.01 sdDyns

  --print $ std [10,2,38,23,38,23,21]
  --print $ tTest "test" [1..10] 6 0.05
  --let t = tTest [1, 1, 1, 1] 5 0.05
  --print t 
  --print $ prop_yampa_sir g [Infected] --[Infected,Recovered,Recovered]

  -- defaultMain $ sirYampaPropTests g