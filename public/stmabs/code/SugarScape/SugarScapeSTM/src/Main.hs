module Main where

import           System.IO
import           System.Random

import           Control.Concurrent.STM
import           Control.Monad.Random
import           Data.Time.Clock
import           FRP.BearRiver

--import           GlossRunner
import           Init
import           Model
import           Simulation

durationSecs :: Double
durationSecs = 60

-- NOTE run with: clear & stack exec -- SugarScapeSTM +RTS -N4 -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount = 500
      envSize    = (128, 128)

      -- initial RNG
      g0                     = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial model for Gloss = output of each simulation step to be rendered
      --initOut                = (0, initEnv, [])
      -- initial simulation state
      (initAis, _)           = unzip initAs

  envVar <- newTVarIO initEnv
  aidVar <- newTVarIO $ maximum initAis

  let sugCtx = SugContext {
      sugCtxEnv     = envVar
    , sugCtxNextAid = aidVar
    }

  (dtVars, aoVars, g') <- spawnAgents initAs g sugCtx
  
  start <- getCurrentTime

  -- initial simulation context
  let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0

  --runWithGloss durationSecs dt initSimCtx sugCtx initOut
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
    then return ()
    else simulate dt simCtx' sugCtx