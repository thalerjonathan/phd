module Main where

import           System.IO
import           System.Random

import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats
import           Control.Monad.Random
import           Data.Time.Clock
import           FRP.BearRiver

import           Discrete
import           Environment
import           GlossRunner
import           Init
import           Model
import           Simulation

durationSecs :: Double
durationSecs = 60

-- NOTE run with: clear & stack exec -- SugarScapeSTMTArray +RTS -N4 -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let stmStatsFlag = False
      envConc      = True  -- runs the environment agent concurrently
      glossOut     = False
      rngSeed      = 42
      dt           = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      agentCount   = 500
      envSize      = (50, 50)
      -- initial RNG
      g0                     = mkStdGen rngSeed

  -- initial agents and environment
  let ret                = runRandT (createSugarScape agentCount envSize) g0
  ((initAs, initEnv), g) <- atomically ret
  envCells               <- atomically $ allCellsWithCoords initEnv

  -- initial model for Gloss = output of each simulation step to be rendered
  let initOut                = (0, envCells, [])
  -- initial simulation state
  let (initAis, _)           = unzip initAs
 
  aidVar <- newTVarIO $ maximum initAis

  let sugCtx = SugContext {
      sugCtxEnv     = initEnv
    , sugCtxNextAid = aidVar
    }

  start <- getCurrentTime

  let initAs' = if envConc then (0, sugEnvironment) : initAs else initAs
      envAg   = if envConc then Nothing else Just sugEnvironment

  (dtVars, aoVars, g') <- spawnAgents initAs' g sugCtx stmStatsFlag
  -- initial simulation context
  let initSimCtx = mkSimContex dtVars aoVars 0 g' start 0 envAg

  if glossOut
    then runWithGloss durationSecs dt initSimCtx sugCtx initOut stmStatsFlag
    else simulate dt initSimCtx sugCtx stmStatsFlag

simulate :: RandomGen g
         => DTime
         -> SimContext g
         -> SugContext
         -> Bool
         -> IO ()
simulate dt simCtx sugCtx stmStatsFlag = do
  (simCtx', (t, _, aos)) <- simulationStep dt sugCtx simCtx stmStatsFlag

  -- NOTE: need to print t otherwise lazy evaluation would omit all computation
  putStrLn $ "t = " ++ show t ++ " agents = " ++ show (length aos)
  
  ret <- checkTime durationSecs simCtx' 
  if ret 
    then do
      dumpSTMStats
      putStrLn "goodbye" 
    else simulate dt simCtx' sugCtx stmStatsFlag