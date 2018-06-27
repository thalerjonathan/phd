module Main where

import            Data.IORef
import            System.Random

import            Control.Monad.Random
import            Control.Monad.Reader
import            Control.Monad.State.Strict
import            FRP.BearRiver
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Simulate

import AgentMonad
import Init
import Model
import Renderer

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

      g0 = mkStdGen rngSeed
  
      ((initAs, initEnv), g) = runRand (createSugarScape agentCount envSize) g0

      initOut = (0, initEnv, [])

      initSimState = mkSimState (simStepSF initAs) mkAbsState initEnv g

  outRef <- newIORef initSimState

  simulateIO 
    (displayGlossWindow winTitle winSize)
    black
    frequency
    initOut
    (modelToPicture winSize)
    (renderStep dt outRef)

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
  putStrLn "renderStep"
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
  res <- mapM (`unMSF` AgentIn) sfs

  -- TODO: agent creation / destruction goes in here

  let obs = fmap (\(ao, _) -> (0, aoObservable ao)) res
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