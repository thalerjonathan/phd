module Main where

import System.Random

import Control.Monad.Random

import SIR.SIR
import SIR.Export

seed :: Int
seed = 42

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 1

-- use negative number for unlimited number of events
maxEvents :: Integer
maxEvents = -1 

-- use 1 / 0 for unrestricted time
maxTime :: Double
maxTime = 150.0

main :: IO ()
main = do
  -- let g0 = mkStdGen seed
  g0 <- getStdGen

  let as = initAgents agentCount infectedCount

      absRand = runABS as (0, 0, 0) maxEvents maxTime 1.0
      ((t, evtCount, ss), _) = runRand absRand g0

  print $ "Finished at t = " ++ show t ++ 
          ", after " ++ show evtCount ++ 
          " events"
          
  let ss' = map (\(s, i, r) -> (fromIntegral s, fromIntegral i, fromIntegral r)) ss
  writeDynamicsToFile ("sir-event" ++ show agentCount ++ "agents.m") ss'