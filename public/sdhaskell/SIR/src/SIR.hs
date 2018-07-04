{-# LANGUAGE Arrows #-}
module SIR
  (
    sir
  ) where

import FRP.Yampa

sir :: Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> SF () (Double, Double, Double)
sir populationSize infectedCount contactRate infectivity illnessDuration
    = loopPre (initSus, initInf, initRec) sir'
  where
    initSus = populationSize - infectedCount
    initInf = infectedCount
    initRec = 0

    sir' :: SF 
            ((), (Double, Double, Double))
            ((Double, Double, Double), (Double, Double, Double))
    sir' = proc (_, (s, i, _r)) -> do
      let infectionRate  = (i * contactRate * s * infectivity) / populationSize
      let recoveryRate  = i / illnessDuration

      s' <- (initSus+) ^<< integral -< (-infectionRate)
      i' <- (initInf+) ^<< integral -< (infectionRate - recoveryRate)
      r' <- (initRec+) ^<< integral -< recoveryRate

      returnA -< dupe (s', i', r')

    dupe :: a -> (a, a)
    dupe a = (a, a)