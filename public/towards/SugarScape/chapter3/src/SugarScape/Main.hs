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
            | Visual Int AgentVis SiteVis
            deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let sugParams = mkParamsFigureIV_3
      output    = Export 1000 -- Export 1000 -- Visual 0 Default Resource
      rngSeed   = Just 42                    -- Nothing -- Just 42

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case output of 
    Pure   steps     -> print $ simulateUntil steps initSimState
    Export steps     -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual sps av cv -> runGloss initSimState (0, 0, initEnv, []) sps av cv