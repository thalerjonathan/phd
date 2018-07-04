module PureRunner 
  (
    runSDPure
  ) where

import FRP.Yampa

import SIR

runSDPure :: Double
          -> Double
          -> Double
          -> Double
          -> Double
          -> Time
          -> DTime
          -> [(Double, Double, Double)]
runSDPure populationSize infectedCount contactRate infectivity illnessDuration t dt 
    = embed (sir populationSize infectedCount contactRate infectivity illnessDuration) ((), steps)
  where
    steps = replicate (floor $ t / dt) (dt, Nothing)