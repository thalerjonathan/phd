{-# LANGUAGE Arrows #-}
module SIR
  (
    SIRStep
    
  , sir
  ) where

import FRP.Yampa

type SIRStep = (Double, Double, Double) 

sir :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> SF () SIRStep
sir populationSize infectedCount contactRate infectivity illnessDuration
    = loopPre (initSus, initInf, initRec) sir'
  where
    initSus = populationSize - infectedCount
    initInf = infectedCount
    initRec = 0

    sir' :: SF 
            ((), SIRStep)
            (SIRStep, SIRStep)
    sir' = proc (_, (s, i, _r)) -> do
      let infectionRate  = (i * contactRate * s * infectivity) / populationSize
      let recoveryRate  = i / illnessDuration

      s' <- (initSus+) ^<< integral -< (-infectionRate)
      i' <- (initInf+) ^<< integral -< (infectionRate - recoveryRate)
      r' <- (initRec+) ^<< integral -< recoveryRate

      returnA -< dupe (s', i', r')

    dupe :: a -> (a, a)
    dupe a = (a, a)