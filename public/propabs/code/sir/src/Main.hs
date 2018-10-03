module Main where

import System.Random

import Test.Tasty

import SIRYampaTests

seed :: Int
seed = 42

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g = mkStdGen seed
  setStdGen g

  --let t = tTest [1..10] 5 0.05
  --print t 

  defaultMain $ sirYampaTests g