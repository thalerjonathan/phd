{-# LANGUAGE Arrows #-}
module SIR.SD
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
         -> [(Double, (Double, Double, Double))]
runSIRSD s0 i0 r0 contactRate infectivity illnessDuration tMax dt 
    = embed sirSF ((), steps)
  where
    n     = s0 + i0 + r0
    steps = replicate (floor $ tMax / dt) (dt, Nothing)

    sirSF :: SF () (Double, (Double, Double, Double))
    sirSF = loopPre (s0, i0, r0) sirFeedback
      where
        sirFeedback :: SF 
                      ((), (Double, Double, Double))
                      ((Double, (Double, Double, Double)), (Double, Double, Double))
        sirFeedback = proc (_, (s, i, _r)) -> do
          let infectionRate = (i * contactRate * s * infectivity) / n
              recoveryRate  = i / illnessDuration

          s' <- (s0+) ^<< integral -< (-infectionRate)
          i' <- (i0+) ^<< integral -< (infectionRate - recoveryRate)
          r' <- (r0+) ^<< integral -< recoveryRate

          t <- time -< ()
          let sir = (s', i', r')
          
          returnA -< ((t, sir), sir)