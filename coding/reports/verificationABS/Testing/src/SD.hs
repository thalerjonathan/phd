{-# LANGUAGE Arrows #-}
module SD where

import FRP.Yampa

import SIR

runSD :: [(Double, Double, Double)]
runSD = embed sir ((), steps)
  where
    n     = t / dt
    steps = replicate (floor n) (dt, Nothing)

sir :: SF () (Double, Double, Double)
sir = loopPre (initSus, initInf, initRec) sir'
  where
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

    initSus = populationSize - infectedCount
    initInf = infectedCount
    initRec = 0