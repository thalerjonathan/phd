module Main where

import            System.IO
import            System.Random

import            Control.Monad.Random
import            Data.Time.Clock
import            FRP.BearRiver

import            AgentMonad
-- import            GlossRunner
import            Init
import            Simulation

durationSecs :: Double
durationSecs = 60

-- NOTE run with clear & stack exec -- SugarScapePure +RTS -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount = 500
      envSize    = (51, 51)

      -- initial RNG
      g0 = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial simulation state
      (initAis, initSfs) = unzip initAs

  start <- getCurrentTime

  let initSimState = mkSimState (simStepSF initAis initSfs) (mkAbsState $ maximum initAis) initEnv g start 0

  simulate dt initSimState
  --runWithGloss durationSecs dt initSimState (0, initEnv, [])

simulate :: RandomGen g
         => DTime
         -> SimulationState g
         -> IO ()
simulate dt ss = do
  (ss', (t, _, aos)) <- simulationStep dt ss

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  
  ret <- checkTime durationSecs ss' 
  if ret 
    then return ()
    else simulate dt ss'