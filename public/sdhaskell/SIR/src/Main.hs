module Main where

import PureRunner

main :: IO ()
main = do
  let populationSize  = 1000
      infectedCount   = 1
      contactRate     = 5
      infectivity     = 0.05
      illnessDuration = 15

      t               = 200
      dt              = 0.001

  let ret = runSDPure populationSize infectedCount contactRate infectivity illnessDuration t dt

  print ret