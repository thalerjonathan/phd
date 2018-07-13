module GlossRunner
  (
    runWithGloss
  ) where

import            Data.IORef

import            Control.Monad.Random
import            FRP.BearRiver
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Simulate

import            Renderer
import            Simulation

runWithGloss :: RandomGen g
             => DTime
             -> SimulationState g
             -> SimStepOut
             -> IO ()
runWithGloss dt initSimState initOut = do
  let frequency  = 1
      winSize    = (800, 800)
      winTitle   = "SugarScape"

      -- initial model for Gloss = output of each simulation step to be rendered
      -- initOut = (0, initEnv, [])
      
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