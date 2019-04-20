module SIRGenerators where

import Control.Monad.Random
import Test.Tasty.QuickCheck as QC

import SIR.SIR

genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genSIRState :: Gen SIRState
genSIRState = elements [Susceptible, Infected, Recovered]

genSIR :: Double
       -> Double
       -> [SIRState] 
       -> Double
       -> Double
       -> Double
       -> Gen [(Int, Int, Int)]
genSIR tMax dt as cor inf ild 
  = runSIRFor tMax dt as cor inf ild <$> genStdGen