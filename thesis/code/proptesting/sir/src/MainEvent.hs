module Main where

import System.Random

import Export.Compress
import Export.Matlab
import SIR.Event
import SIR.Model

seed :: Int
seed = 42

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 1

contactRate :: Double
contactRate = 5

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

-- use negative number for unlimited number of events
maxEvents :: Integer
maxEvents = -1

-- use 1 / 0 for unrestricted time
maxTime :: Double
maxTime = 150.0

main :: IO ()
main = do
  let g0 = mkStdGen seed
  --g0 <- getStdGen

  let ss0 = replicate (agentCount - infectedCount) Susceptible ++ 
            replicate infectedCount Infected

      (ss, evtCnt) = runEventSIR ss0 contactRate infectivity illnessDuration maxEvents maxTime g0 

  print $ "Finished at t = " ++ show (fst $ last ss) ++ 
          ", after " ++ show evtCnt ++ 
          " events"

  let ssCompr = compressOutput ss

  let ssCompr' = map (\(t, (s, i, r)) -> (t, (fromIntegral s, fromIntegral i, fromIntegral r))) ssCompr
  writeMatlabFile "sir-event.m" ssCompr'