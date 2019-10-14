module Utils.GenTimeSIR where

import Control.Monad.Random
import FRP.Yampa
import Test.Tasty.QuickCheck

import SIR.Model
import SIR.Time
import Utils.GenSIR

genAgent :: (StdGen -> SIRAgent)
         -> [SIRState]
         -> Double
         -> Double
         -> Gen [SIRState]
genAgent a as tMax dt = do
  g <- genStdGen

  let ag  = a g
      n   = floor (tMax / dt)
      -- dts = if tMax == 0 then trace "infinite" repeat (dt, Nothing) else trace ("steps = " ++ show n) replicate n (dt, Nothing)
      dts = if tMax == 0 then repeat (dt, Nothing) else replicate n (dt, Nothing)
      aos = embed ag (as, dts)

  return aos 

genSusceptible :: Double
               -> Double 
               -> Double
               -> [SIRState]
               -> Double
               -> Double
               -> Gen [SIRState]
genSusceptible cor inf ild as tMax dt = do
  let a = susceptibleAgent cor inf ild
  genAgent a as tMax dt

genInfected :: Double
            -> [SIRState]
            -> Double
            -> Double
            -> Gen [SIRState]
genInfected ild as tMax dt = do
  let a = infectedAgent ild
  genAgent a as tMax dt

genRecovered :: [SIRState]
             -> Double
             -> Double
             -> Gen [SIRState]
genRecovered as tMax dt = do
  let a = const recoveredAgent
  genAgent a as tMax dt
  
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