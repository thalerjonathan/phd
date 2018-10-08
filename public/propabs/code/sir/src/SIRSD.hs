{-# LANGUAGE Arrows #-}
module SIRSD
  ( runSD
  ) where

import FRP.Yampa as Yampa

runSD :: Double
      -> Double
      -> Double

      -> Double
      -> Double
      -> Double

      -> Yampa.Time
      -> Yampa.DTime
      -> [(Double, Double, Double)]
runSD initSus 
      initInf 
      initRec

      contactRate
      infectivity
      illnessDuration

      t dt 
    = Yampa.embed sir ((), steps)
  where
    n     = initSus + initInf + initRec
    steps = replicate (floor $ t / dt) (dt, Nothing)

    sir :: Yampa.SF () (Double, Double, Double)
    sir = Yampa.loopPre (initSus, initInf, initRec) sir'
      where
        sir' :: Yampa.SF 
                ((), (Double, Double, Double))
                ((Double, Double, Double), (Double, Double, Double))
        sir' = proc (_, (s, i, _r)) -> do
          let infectionRate  = (i * contactRate * s * infectivity) / n
          let recoveryRate  = i / illnessDuration

          s' <- (initSus+) ^<< Yampa.integral -< (-infectionRate)
          i' <- (initInf+) ^<< Yampa.integral -< (infectionRate - recoveryRate)
          r' <- (initRec+) ^<< Yampa.integral -< recoveryRate

          returnA -< dupe (s', i', r')

        dupe :: a -> (a, a)
        dupe a = (a, a)