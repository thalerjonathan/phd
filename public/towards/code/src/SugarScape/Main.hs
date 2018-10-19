module Main where

import System.IO

import FRP.BearRiver

import SugarScape.ExportRunner
import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Simulation

data Output = Pure Time | Export Time | Visual Int deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let -- sugarscape parameters
      sugParams   = mkParamsAnimationII_7 -- mkParamsAnimationII_7 mkAnimationII_6 mkParamsWealthDistr mkParamsCarryingCapacity mkParamsAnimationII_1 mkParamsAnimationII_2 mkParamsAnimationII_3
      -- rendering output yes/no
      out         = Visual 0 -- Visual -- Export 400
      -- RNG seed
      rngSeed     = Nothing -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case out of 
    Pure   steps       -> print $ simulateUntil steps initSimState
    Export steps       -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual stepsPerSec -> runGloss initSimState (0, initEnv, []) stepsPerSec