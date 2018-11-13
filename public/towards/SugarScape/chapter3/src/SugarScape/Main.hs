module Main where

import System.IO

import SugarScape.Common
import SugarScape.ExportRunner
import SugarScape.GlossRunner
import SugarScape.Model
import SugarScape.Renderer
import SugarScape.Simulation

data Output = Console Time 
            | File Time
            | Visual Int AgentVis SiteVis
            deriving (Eq, Show)

-- TODO: implement optargs

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let sugParams = mkParamsFigureIV_14 
      output    = File 1000              -- File 1000 -- Visual 0 Default Resource
      rngSeed   = Nothing :: (Maybe Int) -- Nothing :: (Maybe Int) -- Just 42

  putStrLn $ "Running Sugarscape with... \n--------------------------------------------------\n" ++ show sugParams ++ "\n--------------------------------------------------"
  putStrLn $ "Output Type: \t\t\t" ++ show output 
  putStrLn $ "RNG Seed: \t\t\t" ++ show rngSeed  ++ "\n--------------------------------------------------"

  (initSimState, initEnv) <- initSimulationOpt rngSeed sugParams

  case output of 
    Console steps    -> print $ simulateUntil steps initSimState
    File   steps     -> writeSimulationUntil "export/dynamics.m" steps initSimState
    Visual sps av cv -> runGloss sugParams initSimState (0, 0, initEnv, []) sps av cv

  putStrLn "\n--------------------------------------------------\n"

{-
Usage sugarscape --scenario STRING [--output Visual | File | Console]
                                   [--outfile STRING] 
                                   [--agentvis Default | Gender | Culture | Tribe | Welfare ]
                                   [--sitevis Resource | Polution ]
                                   [--renderfreq INT] 
                                   [--steps INT] 
                                   [--rng INT]
-}