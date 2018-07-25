module Main where

import            System.IO
import            System.Random

import            Control.Monad.Random
import            Data.Time.Clock
import            FRP.BearRiver

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

  let glossOut   = False
      perfFile   = "50x50_500.txt"
      rngSeed    = 42
      dt         = 1.0
      agentCount = 500
      envSize    = (50, 50)

      -- initial RNG
      (g0, shuffleRng) = split $ mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial simulation state
      (initAis, initSfs) = unzip initAs

  start <- getCurrentTime

  let initSimState = mkSimState (simStepSF initAis initSfs shuffleRng) (mkAbsState $ maximum initAis) initEnv g start 0

  if glossOut
    then runWithGloss durationSecs dt initSimState (0, initEnv, []) perfFile
    else simulate dt initSimState perfFile

simulate :: RandomGen g
         => DTime
         -> SimulationState g
         -> String 
         -> IO ()
simulate dt ss perfFile = do
  (ss', (t, _, aos)) <- simulationStep dt ss

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  --putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  print $ length aos
  
  ret <- checkTime durationSecs ss' perfFile
  if ret 
    then return ()
    else simulate dt ss' perfFile