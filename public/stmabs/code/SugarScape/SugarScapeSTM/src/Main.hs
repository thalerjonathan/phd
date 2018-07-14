module Main where

import           System.IO
import           System.Random

import           Control.Concurrent.STM
import           Control.Monad.Random
import           Data.Time.Clock
import           FRP.BearRiver

import           GlossRunner
import           Environment
import           Init
import           Model
import           Simulation

durationSecs :: Double
durationSecs = 60

-- NOTE run with: clear & stack exec -- SugarScapeSTM +RTS -N4 -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let envConc    = False  -- runs the environment agent concurrently
      rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount = 500
      envSize    = (50, 50)

      -- initial RNG
      g0                     = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial model for Gloss = output of each simulation step to be rendered
      initOut                = (0, initEnv, [])
      -- initial simulation state
      (initAis, _)           = unzip initAs

  envVar <- newTVarIO initEnv
  aidVar <- newTVarIO $ maximum initAis

  let sugCtx = SugContext {
      sugCtxEnv     = envVar
    , sugCtxNextAid = aidVar
    }

  start <- getCurrentTime

  let initAs' = if envConc then (0, sugEnvironment) : initAs else initAs
      envAg   = if envConc then Nothing else (Just sugEnvironment)

  (dtVars, aoVars, g') <- spawnAgents initAs' g sugCtx
  -- initial simulation context
  let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0 envAg
  runWithGloss durationSecs dt initSimCtx sugCtx initOut
  simulate dt initSimCtx sugCtx

simulate :: RandomGen g
         => DTime
         -> SimContext g
         -> SugContext
         -> IO ()
simulate dt simCtx sugCtx = do
  (simCtx', (t, _, _)) <- simulationStep dt sugCtx simCtx

  print t

  ret <- checkTime durationSecs simCtx' 
  if ret 
    then putStrLn "goodbye" 
    else simulate dt simCtx' sugCtx