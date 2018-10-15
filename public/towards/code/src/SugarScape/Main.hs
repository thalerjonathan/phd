module Main where

import System.IO
import System.Random

import Control.Monad.Random

import SugarScape.AgentMonad
import SugarScape.Environment 
import SugarScape.GlossRunner
import SugarScape.Init
import SugarScape.Model
import SugarScape.Simulation

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let -- sugarscape parameters
      sugParams   = mkParamsAnimationII_1

      -- rendering output yes/no
      glossOut    = True
      stepsPerSec = 0
      -- RNG seed
      rngSeed     = 42
      -- initial RNG
      (g0, shuffleRng)       = split $ mkStdGen rngSeed
      -- initial agents and environment data
      ((initAs, initEnv), g) = runRand (createSugarScape sugParams) g0
      -- initial simulation state
      (initAis, initSfs)     = unzip initAs
      -- initial simulation state
      initSimState           = mkSimState 
                                (simStepSF initAis initSfs (sugEnvironment sugParams) shuffleRng) 
                                (mkAbsState $ maximum initAis) initEnv g 0

  if glossOut
    then runGloss initSimState (0, initEnv, []) stepsPerSec
    else print $ simulateUntil 100 initSimState