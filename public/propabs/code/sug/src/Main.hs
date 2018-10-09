module Main where

import            System.IO
import            System.Random

import            Control.Monad.Random

import            AgentMonad
import            GlossRunner
import            Init
import            Simulation

durationSecs :: Double
durationSecs = 60

-- NOTE run with clear & stack exec -- SugarScapePure +RTS -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let glossOut    = True
      rebirthFlag = False -- an agent who dies will schedule to create a new random agent => keeps population (more or less) constant 
      rngSeed     = 42
      agentCount  = 500
      envSize     = (50, 50)

      -- initial RNG
      (g0, shuffleRng) = split $ mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize rebirthFlag) g0
      -- initial simulation state
      (initAis, initSfs) = unzip initAs

  let initSimState = mkSimState (simStepSF initAis initSfs shuffleRng) (mkAbsState $ maximum initAis) initEnv g 0

  if glossOut
    then runGloss initSimState (0, initEnv, [])
    else print $ simulateUntil 100 initSimState