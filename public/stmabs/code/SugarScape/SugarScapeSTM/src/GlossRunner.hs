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
--import           Renderer
import           Simulation

runWithGloss :: RandomGen g
             => Double
             -> DTime
             -> SimContext g
             -> SugContext
             -> SimStepOut
             -> IO ()
runWithGloss durSecs dt initSimCtx sugCtx initOut = do
  let freq     = 0
      winSize  = (800, 800)
      winTitle = "SugarScape"

  outRef <- newIORef initSimCtx

  if freq > 0
    then
      simulateIO 
        (displayGlossWindow winTitle winSize) -- window title and size
        black                     -- background
        freq                      -- how many steps of the simulation to calculate per second (roughly, depends on rendering performance)
        initOut                   -- initial model = output of each simulation step to be rendered
        (modelToPicture winSize)  -- model-to-picture function
        (renderStep durSecs dt sugCtx outRef)    -- 
    else 
      animateIO
        (displayGlossWindow winTitle winSize)
        black
        (renderStepAnimate winSize durSecs dt sugCtx outRef)
        (const $ return ())

  return ()

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SimStepOut
               -> IO GLO.Picture
modelToPicture _winSize _out --(t, env, as) 
  = return GLO.Blank -- $ renderSugarScapeFrame winSize t env as

renderStep :: RandomGen g
           => Double
           -> DTime
           -> SugContext
           -> IORef (SimContext g)
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep durSecs dt sugCtx ssRef _ _ _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx' 

  return out
   
renderStepAnimate :: RandomGen g
                  => (Int, Int)
                  -> Double
                  -> DTime
                  -> SugContext
                  -> IORef (SimContext g)
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize durSecs dt sugCtx ssRef _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx'

  modelToPicture winSize out 