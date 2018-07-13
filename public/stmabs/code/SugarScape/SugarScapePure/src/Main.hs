module Main where

import            System.IO
import            System.CPUTime
import            System.Random

import            Control.Monad.Random
import            FRP.BearRiver

import            AgentMonad
import            Init
--import            GlossRunner
import            Simulation

durationSecs :: Integer
durationSecs = 10

durationPico :: Integer
durationPico = durationSecs * 1000 * 1000 * 1000 * 1000

-- NOTE run with clear & stack exec -- SugarScapePure +RTS -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  start <- getCPUTime

  let rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount = 500
      envSize    = (128, 128)

      -- initial RNG
      g0 = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial simulation state
      (initAis, initSfs) = unzip initAs
      initSimState = mkSimState (simStepSF initAis initSfs) (mkAbsState $ maximum initAis) initEnv g start 0

  simulate dt initSimState

simulate :: RandomGen g
         => DTime
         -> SimulationState g
         -> IO ()
simulate dt ss = do
  (ss', out) <- simulationStep dt ss

  t <- getCPUTime
 
  let start = simStart ss
  let dtStart = seq out (t - start)

  print out

  if dtStart > durationPico
    then (do 
      --print out
      let steps      = simSteps ss'
          stepsRatio = (fromIntegral steps / fromIntegral durationSecs) :: Double

      putStrLn $ "Calculated " ++ show steps ++ " steps with a ratio of " ++ show stepsRatio)
    else simulate dt ss' 
