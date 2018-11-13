module SugarScape.GlossRunner
  ( runGloss
  ) where

import Data.IORef

import Control.Monad.Random
import qualified  Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import SugarScape.Model
import SugarScape.Renderer
import SugarScape.Simulation

runGloss :: RandomGen g
         => SugarScapeParams
         -> SimulationState g
         -> SimStepOut
         -> Int
         -> AgentVis
         -> SiteVis
         -> IO ()
runGloss params initSimState initOut stepsPerSec av cv = do
  let winSize  = (800, 800)
      winTitle = "SugarScape " ++ sgParamsName params
      
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
        (modelToPicture winSize av cv)  -- model-to-picture function
        (renderStep ssRef)    -- 
    else
      animateIO
        (displayGlossWindow winTitle winSize)
        white
        (renderStepAnimate winSize ssRef av cv)
        (const $ return ())

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> AgentVis
               -> SiteVis
               -> SimStepOut
               -> IO GLO.Picture
modelToPicture winSize av cv (t, steps, env, as) 
  = return $ renderSugarScapeFrame winSize t steps env as av cv

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
                  -> AgentVis
                  -> SiteVis
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize ssRef av cv _ = do
  ss <- readIORef ssRef
  let (ss', out) = simulationStep ss
  writeIORef ssRef ss'

  modelToPicture winSize av cv out