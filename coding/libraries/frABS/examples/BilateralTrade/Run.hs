module BilateralTrade.Run (
    runBilateralTrade
  ) where

import Control.Monad.Random

import BilateralTrade.Init
import BilateralTrade.Model

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.1

t :: DTime
t = 150

runBilateralTrade :: IO ()
runBilateralTrade = do
  params <- initSimulation Sequential Nothing Nothing False (Just rngSeed)
    
  (initAdefs, initEnv) <- evalRandIO initBilateralTrade

  let dynamics = simulateTime initAdefs initEnv params dt t

  print dynamics