module Main where

import PureRunner
import VisualRunner

main :: IO ()
main = do
  let populationSize  = 1000
      infectedCount   = 1
      contactRate     = 5
      infectivity     = 0.05
      illnessDuration = 15

      t               = 150
      dt              = 0.01

  let ret = runSDPure populationSize infectedCount contactRate infectivity illnessDuration t dt
  runSDVisual populationSize infectedCount contactRate infectivity illnessDuration t dt

  print ret