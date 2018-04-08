module Main where

import Data.Void

import ABS
import SD
-- import SIR

main :: IO ()
main = do
  let sdDyns  = runSD
      absDyns = runABS
  
  --let filename = "sd_" ++ show populationSize ++ ".m"
  --writeAggregatesToFile filename sdDyns

  print sdDyns
  print absDyns
  
  return ()


-- | Testing behaviour of susceptible agent
--   calculate 1 step, TODO: importance of dt, dt -> 0 should result in correct results
--   with all other agents infected
--   on average makes transition to infected 
--   with prob infectivity, this means
--   we are running this test a large number of 
--   times N, counting the number of infected
--   whhere this number after this
--   step should be within an epsilon of N * infectivity
testSusceptible :: ()
testSusceptible = undefined

-- | Testing behaviour of infected agent
--   run test until agent recovers, which happens
--   on average after illnessDuration
--   we are running this test a larger number of 
--   times N and averaging the durations of all
--   agents until their recovery
--   should be within an epsilon of illnessDuration
testInfected :: ()
testInfected = undefined

-- | Testing behaviour of recovered agent
--   A correct recovered agent will stay recovered
--   forever, which cannot be tested through 
--   computation. We can reason that it is correct
--   simply by looking at the code, which 
--   is so simple (1 line) that it is correct 
--   by definition: its basically a constant function
--   We indicated by Void and undefined that this
--   test does not make sense.
testRecovered :: Void
testRecovered = undefined

-- | Compare the dynamics of ABS and SD approach
--   should be on average the same
--   we are calculating a large number of ABS
--   replications N and average them, this will
--   then be compared to the SD dynamics (note
--   that we only need to calculate the SD 
--   dynamics once, because they are not stochastic)
testDynamics :: ()
testDynamics = undefined