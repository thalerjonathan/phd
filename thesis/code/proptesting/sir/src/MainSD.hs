module Main where

import Export.CSV
import SIR.SD

-- contact rate
beta :: Double
beta = 5

-- infectivity
gamma :: Double
gamma = 0.05

-- illness duration
delta :: Double
delta = 15.0

tLimit :: Double
tLimit = 150

main :: IO ()
main = do
  let ret = runSIRSD 
              999
              1
              0
              beta
              gamma    
              delta     
              tLimit
  --writeMatlabFile "sir-sd.m" ret
  writeCSVFile "sir-sd.csv" ret