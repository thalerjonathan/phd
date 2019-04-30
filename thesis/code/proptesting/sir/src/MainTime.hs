module Main where

import System.Random

import Export.Compress
import Export.CSV
import SIR.Time

seed :: Int
seed = 42

main :: IO ()
main = do
  --let g   = mkStdGen seed
  g <- getStdGen

  let ctx = defaultSIRCtx g
      ret = runTimeSIR ctx

  let retCompr = compressOutput ret
  let ret' = map (\(t, (s, i, r)) -> (t, (fromIntegral s, fromIntegral i, fromIntegral r))) retCompr
  --writeMatlabFile "sir-time.m" ret'
  writeCSVFile "sir-time.csv" ret'