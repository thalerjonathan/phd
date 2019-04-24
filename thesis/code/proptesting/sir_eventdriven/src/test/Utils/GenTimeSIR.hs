module Utils.GenTimeSIR where

import Test.Tasty.QuickCheck

import SIR.Time
import Utils.GenSIR

genTimeSIR :: [SIRState] 
           -> Double
           -> Double
           -> Double
           -> Double
           -> Double
           -> Gen [(Double, (Int, Int, Int))]
genTimeSIR as cor inf ild tMax dt 
  = runTimeSIRFor as cor inf ild tMax dt <$> genStdGen