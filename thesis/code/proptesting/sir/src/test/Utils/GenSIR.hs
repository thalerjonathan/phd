module Utils.GenSIR where

import Control.Monad.Random
import Test.Tasty.QuickCheck

import SIR.Model

newtype Probability = P Double deriving Show

newtype TimeRange = T Double deriving Show

instance Arbitrary SIRState where
  arbitrary = genSIRState

instance Arbitrary Probability where
  arbitrary = P <$> choose (0, 1)

instance Arbitrary TimeRange where
  arbitrary = T <$> choose (0, 50)

genStdGen :: Gen StdGen
genStdGen = mkStdGen <$> choose (minBound, maxBound)

genSIRState :: Gen SIRState
genSIRState = elements [Susceptible, Infected, Recovered]