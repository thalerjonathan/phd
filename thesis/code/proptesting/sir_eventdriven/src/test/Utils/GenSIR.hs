module Utils.GenSIR where

import Control.Monad.Random
import Test.Tasty.QuickCheck

import SIR.Model

genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

genSIRState :: Gen SIRState
genSIRState = elements [Susceptible, Infected, Recovered]
