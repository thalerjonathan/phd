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

-- TODO: implement optargs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let sugParams = mkParamsFigureIV_10 
      output    = Export 1000            -- Export 1000 -- Visual 0 Default Resource
      rngSeed   = Nothing :: (Maybe Int) -- Nothing :: (Maybe Int) -- Just 42

  putStrLn $ "Running Sugarscape with... \n--------------------------------------------------\n" ++ show sugParams ++ "\n--------------------------------------------------"
  putStrLn $ "Output Type: \t\t\t" ++ show output 
  putStrLn $ "RNG Seed: \t\t\t" ++ show rngSeed  ++ "\n--------------------------------------------------"

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case output of 
    Pure   steps     -> print $ simulateUntil steps initSimState
    Export steps     -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual sps av cv -> runGloss initSimState (0, 0, initEnv, []) sps av cv

  putStrLn "\n--------------------------------------------------\n"