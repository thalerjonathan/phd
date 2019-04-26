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
genTimeSIR as cor inf ild dt tMax 
  = runTimeSIRFor as cor inf ild dt tMax <$> genStdGen

genLastTimeSIR :: [SIRState]
               -> Double
               -> Double
               -> Double 
               -> Double
               -> Double
               -> Gen (Double, (Int, Int, Int))
genLastTimeSIR [] _ _ _ _ _ = return (0, (0,0,0))
genLastTimeSIR as cor inf ild dt tMax = do
  ret <- genTimeSIR as cor inf ild dt tMax
  if null ret
    then return (tMax, aggregateSIRStates as)
    else return (last ret)

genTimeSIRRepls :: Int 
                -> [SIRState]
                -> Double
                -> Double
                -> Double 
                -> Double
                -> Double
                -> Gen [(Int, Int, Int)]
genTimeSIRRepls n as cor inf ild dt t
  = map snd <$> vectorOf n (genLastTimeSIR as cor inf ild dt t)