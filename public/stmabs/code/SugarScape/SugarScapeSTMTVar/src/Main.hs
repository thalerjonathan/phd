module Main where

import           System.IO
import           System.Random

import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats
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

-- NOTE run with: clear & stack exec -- SugarScapeSTMTVar +RTS -N4 -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let stmStatsFlag = False -- collects STM statistics. WARNING: reduces performance!
      envConc      = False -- runs the environment agent concurrently
      rebirthFlag  = True  -- an agent who dies will schedule to create a new random agent => keeps population (more or less) constant 
      perfFile     = "50x50_2500_4_core_rebirth.txt"
      glossOut     = False
      rngSeed      = 42
      dt           = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount   = 2500
      envSize      = (50, 50)

      -- initial RNG
      g0           = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize rebirthFlag) g0
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
      envAg   = if envConc then Nothing else Just sugEnvironment

  (dtVars, aoVars, g') <- spawnAgents initAs' g sugCtx stmStatsFlag
  -- initial simulation context
  let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0 envAg

  if glossOut
    then runWithGloss durationSecs dt initSimCtx sugCtx initOut stmStatsFlag perfFile
    else simulate dt initSimCtx sugCtx stmStatsFlag perfFile

simulate :: RandomGen g
         => DTime
         -> SimContext g
         -> SugContext
         -> Bool
         -> String
         -> IO ()
simulate dt simCtx sugCtx stmStatsFlag perfFile = do
  (simCtx', (t, _, aos)) <- simulationStep dt sugCtx simCtx stmStatsFlag

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  
  ret <- checkTime durationSecs simCtx' perfFile
  if ret 
    then when stmStatsFlag dumpSTMStats
    else simulate dt simCtx' sugCtx stmStatsFlag perfFile