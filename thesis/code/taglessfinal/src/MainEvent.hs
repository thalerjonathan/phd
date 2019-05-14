module Main where

import System.Random

import Export.CSV
import SIR.Model
import SIR.Pure

seed :: Int
seed = 42

susceptibleCount :: Int
susceptibleCount = 1000

infectedCount :: Int
infectedCount = 1

contactRate :: Int
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- use negative number for unlimited number of events
maxEvents :: Int
maxEvents = -1

-- use 1 / 0 for unrestricted time
maxTime :: Double
maxTime = 150.0

main :: IO ()
main = do
  let g0 = mkStdGen seed
  --g0 <- getStdGen

  let as = createSIRStates susceptibleCount infectedCount 0 
      
  let ss = runPureSIR 
            as contactRate infectivity illnessDuration maxEvents maxTime g0 

  putStrLn $ "Finished at t = " ++ show (fst $ last ss) ++ 
             ", after " ++ show (length ss) ++ 
             " events"

  writeCSVFile "sir-tagless.csv" ss