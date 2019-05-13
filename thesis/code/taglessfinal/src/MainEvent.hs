module Main where

import System.Random
import Control.Monad.Random

import Export.Compress
import Export.CSV
import SIR.Event
import SIR.Model

seed :: Int
seed = 42

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 1

contactRate :: Int
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
  --let g0 = mkStdGen seed
  g0 <- getStdGen

  print $ runRand testRandT g0
  print $ runRand testMonadRand g0

  let ss0 = replicate (agentCount - infectedCount) Susceptible ++ 
            replicate infectedCount Infected

      ss  = runEventSIR ss0 contactRate infectivity illnessDuration maxEvents maxTime g0 

  print $ "Finished at t = " ++ show (fst $ last ss) ++ 
          ", after " ++ show (length ss) ++ 
          " events"

  let ssCompr = compressOutput ss

  putStrLn $ "Events compressed to " ++ show (length ssCompr) ++ " events."

  writeCSVFile "sir-tagless.csv" ssCompr

testRandT :: RandomGen g => Rand g (Int, Int)
testRandT = do
  r1 <- getRandomR (0, 10)
  r2 <- testMonadRand
  return (r1, r2)

testMonadRand :: MonadRandom m => m Int
testMonadRand = do
  r1 <- getRandomR (0, 10)
  r2 <- testRandT
  return (r1, r2)