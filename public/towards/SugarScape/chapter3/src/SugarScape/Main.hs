module Main where

import System.IO

import SugarScape.Common
import SugarScape.ExportRunner
import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Renderer
import SugarScape.Simulation

data Output = Pure Time 
            | Export Time
            | Visual Int AgentVis SiteVis deriving (Eq, Show)

-- TODO: leaking memory again... (

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let sugParams = mkParamsAnimationIII_6   -- mkParamsFigureIII_4 mkParamsFigureIII_3 mkParamsAnimationIII_1 mkParamsAnimationII_8 mkParamsAnimationII_7 mkAnimationII_6 mkParamsWealthDistr mkParamsCarryingCapacity mkParamsAnimationII_3 mkParamsAnimationII_2 mkParamsAnimationII_1 
      output    = Visual 0 Culture Sugar -- Export 1000 -- Visual 0 Gender Sugar -- Export 1500
      rngSeed   = Nothing               -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case output of 
    Pure   steps     -> print $ simulateUntil steps initSimState
    Export steps     -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual sps av cv -> runGloss initSimState (0, 0, initEnv, []) sps av cv