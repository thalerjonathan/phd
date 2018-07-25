module GlossRunner
  (
    runWithGloss
  ) where

import            Data.IORef

import            Control.Monad.Random
import            FRP.BearRiver
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Animate
import            Graphics.Gloss.Interface.IO.Simulate

import            Renderer
import            Simulation

runWithGloss :: RandomGen g
             => Double
             -> DTime
             -> SimulationState g
             -> SimStepOut
             -> String
             -> IO ()
runWithGloss durSecs dt initSimState initOut perfFile = do
  let freq     = 4
      winSize  = (800, 800)
      winTitle = "SugarScape"
      
  -- intiialize IORef which holds last simulation state
  ssRef <- newIORef initSimState

  if freq > 0
    then
      -- run stimulation, driven by Gloss
      simulateIO 
        (displayGlossWindow winTitle winSize) -- window title and size
        white                     -- background
        freq                      -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
        initOut                   -- initial model = output of each simulation step to be rendered
        (modelToPicture winSize)  -- model-to-picture function
        (renderStep durSecs dt ssRef perfFile)    -- 
    else
      animateIO
        (displayGlossWindow winTitle winSize)
        white
        (renderStepAnimate winSize durSecs dt ssRef perfFile)
        (const $ return ())

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SimStepOut
               -> IO GLO.Picture
--modelToPicture _ _ 
--  = return GLO.Blank
modelToPicture winSize (t, env, as) 
  = return $ renderSugarScapeFrame winSize t env as

renderStep :: RandomGen g
           => Double
           -> DTime
           -> IORef (SimulationState g)
           -> String
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep durSecs dt ssRef perfFile _ _ _ = do
  ss <- readIORef ssRef
  (ss', out) <- simulationStep dt ss
  writeIORef ssRef ss'
  
  _ <- checkTime durSecs ss' perfFile

  return out

renderStepAnimate :: RandomGen g
                  => (Int, Int)
                  -> Double
                  -> DTime
                  -> IORef (SimulationState g)
                  -> String
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize durSecs dt ssRef perfFile _ = do
  ss <- readIORef ssRef
  (ss', out) <- simulationStep dt ss
  writeIORef ssRef ss'

  _ <- checkTime durSecs ss' perfFile

  modelToPicture winSize out 