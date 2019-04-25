module Utils.GenTimeSIR where

import Test.Tasty.QuickCheck

import SIR.Model
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

genLastTimeSIR :: [SIRState]
               -> Double
               -> Double
               -> Double 
               -> Double
               -> Double
               -> Gen (Double, (Int, Int, Int))
genLastTimeSIR [] _ _ _ _ _ = return (0, (0,0,0))
genLastTimeSIR as cor inf ild tMax dt = do
  ret <- genTimeSIR as cor inf ild tMax dt
  if null ret
    then return (tMax, aggregateSIRStates as)
    else return (last ret)