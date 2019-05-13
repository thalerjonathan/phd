module Main where

import System.Random

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
maxEvents :: Int
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

      ss  = runEventSIR ss0 contactRate infectivity illnessDuration maxEvents maxTime g0 

  putStrLn $ "Finished at t = " ++ show (fst $ last ss) ++ 
             ", after " ++ show (length ss) ++ 
             " events"

  writeCSVFile "sir-tagless.csv" ss

-- testRandT :: RandomGen g => Rand g Int
-- testRandT = do
--   r1 <- getRandomR (0, 10)
--   return r1

-- testMonadRand :: MonadRandom m => m (Int, Int)
-- testMonadRand = do
--   r1 <- getRandomR (0, 10)
--   r2 <- runRandInMonadRandom testRandT
--   return (r1, r2)

-- runRandInMonadRandom :: (MonadState g m, MonadRandom m) => Rand g a -> m a
-- runRandInMonadRandom act = do
--   g <- get
--   let (as', g') = runRand act g
--   put g'
--   return as'