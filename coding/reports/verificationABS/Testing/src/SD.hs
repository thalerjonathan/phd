{-# LANGUAGE Arrows #-}
module SD 
  (
    runSD
  ) where

import FRP.Yampa

runSD :: Double
      -> Double
      -> Double
      -> Double
      -> Double
      -> Time
      -> DTime
      -> [(Double, Double, Double)]
runSD 
      populationSize 
      infectedCount 
      contactRate
      infectivity
      illnessDuration
      t dt 
    = embed (sir populationSize infectedCount) ((), steps)
  where
    steps = replicate (floor $ t / dt) (dt, Nothing)

    sir :: Double 
        -> Double
        -> SF () (Double, Double, Double)
    sir populationSize infectedCount 
        = loopPre (initSus, initInf, initRec) sir'
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