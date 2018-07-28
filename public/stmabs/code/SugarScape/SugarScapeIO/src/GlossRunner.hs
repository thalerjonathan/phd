module GlossRunner 
  (
    runWithGloss
  ) where

import           Data.IORef
import           System.Random

import           FRP.BearRiver
import qualified Graphics.Gloss as GLO
import           Graphics.Gloss.Interface.IO.Animate
import           Graphics.Gloss.Interface.IO.Simulate

import           Model
import           Renderer
import           Simulation

runWithGloss :: RandomGen g
             => Double
             -> DTime
             -> SimContext g
             -> SugContext
             -> SimStepOut
             -> String 
             -> IO ()
runWithGloss durSecs dt initSimCtx sugCtx initOut perfFile = do
  let freq     = 0
      winSize  = (800, 800)
      winTitle = "SugarScape"

  outRef <- newIORef initSimCtx

  if freq > 0
    then
      simulateIO 
        (displayGlossWindow winTitle winSize) -- window title and size
        white                     -- background
        freq                      -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
        initOut                   -- initial model = output of each simulation step to be rendered
        (modelToPicture winSize)  -- model-to-picture function
        (renderStep durSecs dt sugCtx outRef perfFile)    -- 
    else 
      animateIO
        (displayGlossWindow winTitle winSize)
        white
        (renderStepAnimate winSize durSecs dt sugCtx outRef perfFile)
        (const $ return ())

  return ()

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SimStepOut
               -> IO GLO.Picture
-- modelToPicture _ _ 
--  = return GLO.Blank 
modelToPicture winSize (t, env, as) 
  = return $ renderSugarScapeFrame winSize t env as

renderStep :: RandomGen g
           => Double
           -> DTime
           -> SugContext
           -> IORef (SimContext g)
           -> String
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep durSecs dt sugCtx ssRef perfFile _ _ _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx' perfFile

  return out
   
renderStepAnimate :: RandomGen g
                  => (Int, Int)
                  -> Double
                  -> DTime
                  -> SugContext
                  -> IORef (SimContext g)
                  -> String
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize durSecs dt sugCtx ssRef perfFile _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx' perfFile

  modelToPicture winSize out 