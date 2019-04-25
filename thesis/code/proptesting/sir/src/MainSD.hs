module Main where

import Export.Matlab
import SIR.SD

contactRate :: Double
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

tLimit :: Double
tLimit = 150

dt :: Double
dt = 0.1

main :: IO ()
main = do
  let ret = runSIRSD 999 1 0 contactRate infectivity illnessDuration tLimit dt
  writeMatlabFile "sir-sd.m" ret