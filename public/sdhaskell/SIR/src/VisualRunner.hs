module VisualRunner 
  (
    runSDVisual
  ) where

import FRP.Yampa

import SIR

runSDVisual :: Double
            -> Double
            -> Double
            -> Double
            -> Double
            -> Time
            -> DTime
            -> IO ()
runSDVisual populationSize infectedCount contactRate infectivity illnessDuration t dt = do
    let ret = embed (sir populationSize infectedCount contactRate infectivity illnessDuration) ((), steps)
    print ret
  where
    steps = replicate (floor $ t / dt) (dt, Nothing)