module Main where

import           Data.Maybe
import           Data.IORef
import           System.IO
import           System.Random

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats
import           Control.Monad.Random
import           Control.Monad.Reader
import           FRP.BearRiver
import qualified Graphics.Gloss as GLO
import           Graphics.Gloss.Interface.IO.Simulate

import           Common
import           Init
import           Model
import           Renderer

type SimStepOut = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

data SimContext g = SimContext
  { simCtxDtVars :: [MVar DTime]
  , simCtxAoVars :: [MVar (AgentId, SugAgentOut g)]
  , simCtxTime   :: Time
  , simCtxRng    :: g
  }

-- NOTE run with: clear & stack exec -- SugarScapeSTM +RTS -N4 -s

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      frequency  = 1
      winSize    = (800, 800)
      winTitle   = "SugarScape"
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

  (dtVars, aoVars, g') <- spawnAgents initAs g sugCtx

  -- initial simulation context
  let initSimCtx = mkSimContex dtVars aoVars 0 g'
  -- intiialize IORef which holds last update of simulation context
  outRef <- newIORef initSimCtx

  -- run stimulation, driven by Gloss
  simulateIO 
    (displayGlossWindow winTitle winSize) -- window title and size
    black                     -- background
    frequency                 -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
    initOut                   -- initial model = output of each simulation step to be rendered
    (modelToPicture winSize)  -- model-to-picture function
    (renderStep dt sugCtx outRef)    -- 

  return ()

spawnAgents :: RandomGen g
            => [(AgentId, SugAgent g)]
            -> g
            -> SugContext
            -> IO ([MVar DTime], [MVar (AgentId, SugAgentOut g)], g)
spawnAgents [] g0 _ = return ([], [], g0)
spawnAgents ((aid, a) : as) g0 sugCtx = do
  let (g', g'') = split g0

  dtVar  <- newEmptyMVar 
  aoVar <- createAgentThread dtVar g' sugCtx aid a

  (dtVars, aoVars, g) <- spawnAgents as g'' sugCtx

  return (dtVar : dtVars, aoVar : aoVars, g)

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SimStepOut
               -> IO GLO.Picture
modelToPicture winSize (t, env, as) 
  = return $ renderSugarScapeFrame winSize t env as

renderStep :: RandomGen g
           => DTime
           -> SugContext
           -> IORef (SimContext g)
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep dt sugCtx ssRef _ _ _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx
  writeIORef ssRef simCtx'
  return out

simulationStep :: RandomGen g
               => DTime
               -> SugContext
               -> SimContext g
               -> IO (SimContext g, SimStepOut)
simulationStep dt sugCtx simCtx = do
  let dtVars = simCtxDtVars simCtx
      aoVars = simCtxAoVars simCtx
      t      = simCtxTime simCtx
      g      = simCtxRng simCtx

  -- TODO: to reduce STM Retries: schedule Environment in main thread

  -- tell all threads to continue with the corresponding DTime
  mapM_ (`putMVar` dt) dtVars
  -- wait for results
  aos <- mapM takeMVar aoVars
  -- read the latest environment
  env <- readTVarIO $ sugCtxEnv sugCtx

  let newAs = concatMap (\(_, ao) -> sugAoNew ao) aos
      obs   = foldr (\(aid, ao) acc -> 
        if isObservable ao 
          then (aid, fromJust $ sugAoObservable ao) : acc  
          else acc) [] aos

  (newDtVars, newAoVars, g') <- spawnAgents newAs g sugCtx

  let (dtVars', aoVars') = foldr (\((_, ao), dv, aov) acc@(accDtVars, accAoVars) -> 
        if isDead ao 
          then acc 
          else (dv : accDtVars, aov : accAoVars))  ([], []) (zip3 aos dtVars aoVars)

  let dtVars'' = dtVars' ++ newDtVars
      aoVars'' = aoVars' ++ newAoVars
      t'      = t + dt
      simCtx' = mkSimContex dtVars'' aoVars'' t' g'

  --let mt = mod (floor t') 10 :: Integer
  --when (mt == 0) dumpSTMStats
  dumpSTMStats
  
  return (simCtx', (t, env, obs))

stmConf :: TrackSTMConf
stmConf = defaultTrackSTMConf {
    tryThreshold   = Nothing
  , globalTheshold = Nothing
  }

createAgentThread :: RandomGen g 
                  => MVar DTime
                  -> g
                  -> SugContext
                  -> AgentId
                  -> SugAgent g
                  -> IO (MVar (AgentId, SugAgentOut g))
createAgentThread dtVar rng0 sugCtx aid a = do
    -- create the var where the result will be posted to
    retVar <- newEmptyMVar
    _ <- forkIO $ agentThread a rng0 retVar
    return retVar
  where
    agentThread :: RandomGen g 
                => SugAgent g
                -> g
                -> MVar (AgentId, SugAgentOut g)
                -> IO ()
    agentThread sf rng retVar = do
      -- wait for next dt to compute next step
      dt <- takeMVar dtVar

      -- compute next step
      let sfDtReader  = unMSF sf SugAgentIn
          sfCtxReader = runReaderT sfDtReader dt
          sfRand      = runReaderT sfCtxReader sugCtx
          sfSTM       = runRandT sfRand rng
      ((ao, sf'), rng') <- trackSTMConf stmConf "SugarScape" sfSTM -- atomically sfSTM -- trackSTM sfSTM
      -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently

      -- post result to main thread
      putMVar retVar (aid, ao)

      if isDead ao 
        then return ()
        else agentThread sf' rng' retVar

mkSimContex :: [MVar DTime]
            -> [MVar (AgentId, SugAgentOut g)]
            -> Time
            -> g
            -> SimContext g
mkSimContex dtVars aoVars t g = SimContext { 
    simCtxDtVars = dtVars
  , simCtxAoVars = aoVars
  , simCtxTime   = t
  , simCtxRng    = g
  }