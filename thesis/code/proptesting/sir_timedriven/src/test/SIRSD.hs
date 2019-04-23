{-# LANGUAGE Arrows #-}
module SIRSD
  ( runSIRSD
  ) where

import FRP.Yampa

runSIRSD :: Double
         -> Double
         -> Double

         -> Double
         -> Double
         -> Double

         -> Time
         -> DTime
         -> [(Double, Double, Double)]
runSIRSD initSus 
      initInf 
      initRec

      contactRate
      infectivity
      illnessDuration

      t dt 
    = embed sir ((), steps)
  where
    n     = initSus + initInf + initRec
    steps = replicate (floor $ t / dt) (dt, Nothing)

    sir :: SF () (Double, Double, Double)
    sir = loopPre (initSus, initInf, initRec) sir'
      where
        sir' :: SF 
                ((), (Double, Double, Double))
                ((Double, Double, Double), (Double, Double, Double))
        sir' = proc (_, (s, i, _r)) -> do
          let infectionRate  = (i * contactRate * s * infectivity) / n
          let recoveryRate  = i / illnessDuration

          s' <- (initSus+) ^<< integral -< (-infectionRate)
          i' <- (initInf+) ^<< integral -< (infectionRate - recoveryRate)
          r' <- (initRec+) ^<< integral -< recoveryRate

          returnA -< dupe (s', i', r')

        dupe :: a -> (a, a)
        dupe a = (a, a)