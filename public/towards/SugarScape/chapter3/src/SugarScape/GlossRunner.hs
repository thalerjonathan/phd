module SugarScape.GlossRunner
  ( runGloss
  ) where

import            Data.IORef

import            Control.Monad.Random
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Animate
import            Graphics.Gloss.Interface.IO.Simulate

import            SugarScape.Renderer
import            SugarScape.Simulation

runGloss :: RandomGen g
         => SimulationState g
         -> SimStepOut
         -> Int
         -> CellVisualisation
         -> IO ()
runGloss initSimState initOut stepsPerSec cv = do
  let winSize  = (800, 800)
      winTitle = "SugarScape"
      
  -- intiialize IORef which holds last simulation state
  ssRef <- newIORef initSimState

  if stepsPerSec > 0
    then
      -- run stimulation, driven by Gloss
      simulateIO 
        (displayGlossWindow winTitle winSize) -- window title and size
        white                     -- background
        stepsPerSec               -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
        initOut                   -- initial model = output of each simulation step to be rendered
        (modelToPicture winSize cv)  -- model-to-picture function
        (renderStep ssRef)    -- 
    else
      animateIO
        (displayGlossWindow winTitle winSize)
        white
        (renderStepAnimate winSize ssRef cv)
        (const $ return ())

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> CellVisualisation
               -> SimStepOut
               -> IO GLO.Picture
modelToPicture winSize cv (t, steps, env, as) 
  = return $ renderSugarScapeFrame winSize t steps env as cv

renderStep :: RandomGen g
           => IORef (SimulationState g)
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep ssRef _ _ _ = do
  ss <- readIORef ssRef
  let (ss', out) = simulationStep ss
  writeIORef ssRef ss'
  
  return out

renderStepAnimate :: RandomGen g
                  => (Int, Int)
                  -> IORef (SimulationState g)
                  -> CellVisualisation
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize ssRef cv _ = do
  ss <- readIORef ssRef
  let (ss', out) = simulationStep ss
  writeIORef ssRef ss'

  modelToPicture winSize cv out 