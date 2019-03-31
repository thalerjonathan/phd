module Main where

import System.Random

import SIR.SIR
import SIR.Export

seed :: Int
seed = 42

main :: IO ()
main = do
  let g   = mkStdGen seed
      ctx = defaultSIRCtx g
      ret = runSIR ctx

  writeAggregatesToFile "sir.m" (syCtxTimeDelta ctx) ret