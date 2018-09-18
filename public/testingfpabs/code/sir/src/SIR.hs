module SIR 
  (
    SIRState (..)
  , aggregateAllStates
  , aggregateStates
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

data SIRState 
  = Susceptible 
  | Infected 
  | Recovered 
  deriving (Show, Eq)

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]

aggregateAllStates :: [[SIRState]] -> [(Double, Double, Double)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter (Susceptible==) as
    infectedCount = fromIntegral $ length $ filter (Infected==) as
    recoveredCount = fromIntegral $ length $ filter (Recovered==) as