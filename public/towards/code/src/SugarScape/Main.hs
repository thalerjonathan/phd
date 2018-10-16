module Main where

import System.IO

import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Simulation

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let -- sugarscape parameters
      sugParams   = mkParamsAnimationII_1 -- mkParamsCarryingCapacity mkParamsAnimationII_1
      -- rendering output yes/no
      glossOut    = True
      -- steps per second if rendering output
      stepsPerSec = 0
      -- RNG seed
      rngSeed     = Nothing -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  if glossOut
    then runGloss initSimState (0, initEnv, []) stepsPerSec
    else print $ simulateUntil 100 initSimState