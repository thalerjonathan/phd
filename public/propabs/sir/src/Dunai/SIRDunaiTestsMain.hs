module Main where

import System.Random

import Test.Tasty

import SIR
import SIRDunai
import SIRSDDunai
import SIRDunaiTests
import StatsUtils

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g  = mkStdGen seed
  setStdGen g

{-
  let ctx     = defaultSIRCtx g
      absDyns = runSIRDunai ctx
  writeAggregatesToFile "dunaiABS.m" 0.01 absDyns

  let sdDyns = runDunaiSD 999 1 0 5 0.05 15 200 0.01
  writeAggregatesToFile "dunaiSD.m" 0.01 sdDyns
-}

  --print $ std [10,2,38,23,38,23,21]
  --print $ tTest "test" [1..10] 5 0.05
  --let t = tTest [1, 1, 1, 1] 5 0.05
  --print t 
  
  --print $ prop_dunai_sir g [Infected] --[Infected,Recovered,Recovered]

  defaultMain $ sirDunaiPropTests g