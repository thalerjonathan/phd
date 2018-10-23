module Main where

import System.IO

import FRP.BearRiver

import SugarScape.ExportRunner
import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Renderer
import SugarScape.Simulation

data Output = Pure Time 
            | Export Time
            | Visual Int CellVisualisation deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let sugParams = mkParamsAnimationII_8 -- mkParamsAnimationII_8 mkParamsAnimationII_7 mkAnimationII_6 mkParamsWealthDistr mkParamsCarryingCapacity mkParamsAnimationII_3 mkParamsAnimationII_2 mkParamsAnimationII_1 
      output    = Visual 0 Polution -- Visual 0 Sugar -- Export 400
      rngSeed   = Nothing -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case output of 
    Pure   steps  -> print $ simulateUntil steps initSimState
    Export steps  -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual sps cv -> runGloss initSimState (0, 0, initEnv, []) sps cv