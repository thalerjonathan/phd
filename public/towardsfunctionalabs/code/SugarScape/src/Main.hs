module Main where

import            Data.Maybe
import            Data.IORef
import            System.Random

import            Control.Monad.Random
import            Control.Monad.Reader
import            Control.Monad.State.Strict
import            FRP.BearRiver
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Simulate

import            AgentMonad
import            Init
import            Model
import            Renderer

type SimStepOut = (Time, SugEnvironment, [AgentObservable SugAgentObservable])

data SimulationState g = SimulationState 
  { simSf       :: SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
  , simAbsState :: ABSState 
  , simEnv      :: SugEnvironment
  , simRng      :: g
  }

main :: IO ()
main = do
  let rngSeed    = 42
      dt         = 1.0     -- this model has discrete time-semantics with a step-with of 1.0 which is relevant for the aging of the agents
      frequency  = 10
      winSize    = (800, 800)
      winTitle   = "SugarScape"
      agentCount = 400
      envSize    = (50, 50)

      -- initial RNG
      g0 = mkStdGen rngSeed
      -- initial agents and environment
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0
      -- initial model for Gloss = output of each simulation step to be rendered
      initOut = (0, initEnv, [])
      -- initial simulation state
      initSimState = mkSimState (simStepSF initAs) mkAbsState initEnv g

  -- intiialize IORef which holds last simulation state
  outRef <- newIORef initSimState

  -- run stimulation, driven by Gloss
  simulateIO 
    (displayGlossWindow winTitle winSize) -- window title and size
    black                     -- background
    frequency                 -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
    initOut                   -- initial model = output of each simulation step to be rendered
    (modelToPicture winSize)  -- model-to-picture function
    (renderStep dt outRef)    -- 

  return ()

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SimStepOut
               -> IO GLO.Picture
modelToPicture winSize (t, env, as) 
  = return $ renderSugarScapeFrame winSize t env as

renderStep :: RandomGen g
           => DTime
           -> IORef (SimulationState g)
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep dt ssRef _ _ _ = do
  ss <- readIORef ssRef
  (ss', out) <- simulationStep dt ss
  writeIORef ssRef ss'
  return out

simulationStep :: RandomGen g
               => DTime
               -> SimulationState g
               -> IO (SimulationState g, SimStepOut)
simulationStep dt ss = do
  let sf       = simSf ss
      absState = simAbsState ss
      env      = simEnv ss
      g        = simRng ss

  let sfReader   = unMSF sf ()
      sfAbsState = runReaderT sfReader dt
      sfEnvState = runStateT sfAbsState absState
      sfRand     = runStateT sfEnvState env
      ((((out, sf'), absState'), env'), g') = runRand sfRand g

  let t          = absTime absState 
      absState'' = absState' { absTime = t + dt }

  let ss'        = mkSimState sf' absState'' env' g'

  return (ss', (t, env',out))

simStepSF :: RandomGen g
          => [SugAgent g]
          -> SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
simStepSF sfs = MSF $ \_ -> do
  -- TODO: shuffle agent sfs
  -- TODO: keep track of agent-ids
  
  res <- mapM (`unMSF` AgentIn) sfs

  -- TODO: agent creation / destruction goes in here

  let mobs = fmap (\(ao, _) -> aoObservable ao) res
      ais  = replicate (length sfs) 0 
      obs  = zip ais (catMaybes mobs) 
      sfs' = fmap snd res
      ct   = simStepSF sfs'

  return (obs, ct)

mkSimState :: RandomGen g
           => SF (SugAgentMonadT g) () [AgentObservable SugAgentObservable]
           -> ABSState
           -> SugEnvironment
           -> g
           -> SimulationState g
mkSimState sf absState env g = SimulationState 
  { simSf       = sf
  , simAbsState = absState 
  , simEnv      = env
  , simRng      = g
  }