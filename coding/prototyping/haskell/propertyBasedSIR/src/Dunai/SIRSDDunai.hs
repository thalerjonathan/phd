{-# LANGUAGE Arrows #-}
module SIRSDDunai
  ( runDunaiSD
  ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Functor.Identity
import FRP.BearRiver

runDunaiSD :: Double
           -> Double
           -> Double

           -> Double
           -> Double
           -> Double

           -> Time
           -> DTime
           -> [(Double, Double, Double)]
runDunaiSD initSus 
           initInf 
           initRec

           contactRate
           infectivity
           illnessDuration

           t dt 
    = embed' sir ((), steps)
  where
    n     = initSus + initInf + initRec
    steps = replicate (floor $ t / dt) (dt, Nothing)

    sir :: SF Identity () (Double, Double, Double)
    sir = loopPre (initSus, initInf, initRec) sir'
      where
        sir' :: SF Identity
                ((), (Double, Double, Double))
                ((Double, Double, Double), (Double, Double, Double))
        sir' = proc (_, (s, i, _r)) -> do
          let infectionRate  = (i * contactRate * s * infectivity) / n
          let recoveryRate  = i / illnessDuration

          s' <- (initSus+) ^<< integral -< (-infectionRate)
          i' <- (initInf+) ^<< integral -< (infectionRate - recoveryRate)
          r' <- (initRec+) ^<< integral -< recoveryRate

          returnA -< dup (s', i', r')

embed' :: SF Identity a b 
        -> (a, [(DTime, Maybe a)])
        -> [b]
embed' sf0 (a0, samples0) 
    = reverse $ embedAux sf0 a0 samples0 []
  where
    embedAux :: SF Identity a b 
             -> a
             -> [(DTime, Maybe a)]
             -> [b] 
             -> [b]
    embedAux _ _ [] acc = acc 
    embedAux sf aPrev ((dt, ma) : samples) acc
        = embedAux sf' a samples acc' 
      where
        a        = fromMaybe aPrev ma
        sfReader = unMSF sf a
        sfId     = runReaderT sfReader dt
        (b, sf') = runIdentity sfId
        acc'     = b : acc