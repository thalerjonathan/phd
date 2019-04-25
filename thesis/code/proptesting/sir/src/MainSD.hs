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

main :: IO ()
main = do
  let ret = runSIRSD 
              0.1     -- Susceptible
              0.1    -- Infected
              0.1     -- Recovered
              0.1     -- contact rate
              0.55    -- infectivity
              0.1     -- illness duration
              0.1     -- time
  writeMatlabFile "sir-sd.m" ret