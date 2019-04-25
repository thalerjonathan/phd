{-# LANGUAGE Arrows #-}
module SIR.SD
  ( runSIRSD
  ) where

import FRP.Yampa

-- NOTE: the sampling dt parameter is fixed to 0.001 to prevent numeric problems
-- e.g. in case of 0.1 when approaching values close to 0.1 
samplingDelta :: Double
samplingDelta = 0.001

runSIRSD :: Double
         -> Double
         -> Double

         -> Double
         -> Double
         -> Double

         -> Time
         -> [(Double, (Double, Double, Double))]
runSIRSD s0 i0 r0 contactRate infectivity illnessDuration tMax 
    = embed sirSF ((), steps)
  where
    n     = s0 + i0 + r0
    steps = if tMax <= 0
              then repeat (samplingDelta, Nothing) -- infinite stream if no time-limit
              else replicate (floor $ tMax / samplingDelta) (samplingDelta, Nothing)

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