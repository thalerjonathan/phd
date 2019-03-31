module Main where

import System.Random

import Test.Tasty

import SIRTests

seed :: Int
seed = 42

-- clear & stack test --test-arguments="--quickcheck-tests=100 --quickcheck-replay=67991"

main :: IO ()
main = do
  -- fix RNG right from the beginning, to be 
  let g = mkStdGen seed
  setStdGen g

  defaultMain $ sirPropTests g