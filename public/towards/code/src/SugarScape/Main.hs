module Main where

import System.IO

import FRP.BearRiver

import SugarScape.ExportRunner
import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Simulation

data Output = Pure Time | Export Time | Visual deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let -- sugarscape parameters
      sugParams   = mkParamsWealthDistr -- mkParamsWealthDistr mkParamsCarryingCapacity mkParamsAnimationII_1 mkParamsAnimationII_2 mkParamsAnimationII_3
      -- rendering output yes/no
      out         = Export 400 -- Visual -- Export 400
      -- steps per second if rendering output
      stepsPerSec = 0
      -- RNG seed
      rngSeed     = Nothing -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case out of 
    Pure   steps -> print $ simulateUntil steps initSimState
    Export steps -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual -> runGloss initSimState (0, initEnv, []) stepsPerSec