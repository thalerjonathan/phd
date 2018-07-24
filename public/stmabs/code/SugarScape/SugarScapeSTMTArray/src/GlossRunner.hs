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

import           Discrete
import           Model
import           Renderer
import           Simulation

runWithGloss :: RandomGen g
             => Double
             -> DTime
             -> SimContext g
             -> SugContext
             -> SimStepOut
             -> Bool
             -> IO ()
runWithGloss durSecs dt initSimCtx sugCtx initOut stmStatsFlag = do
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
        (modelToPicture winSize sugCtx)  -- model-to-picture function
        (renderStep durSecs dt sugCtx outRef stmStatsFlag)    -- 
    else 
      animateIO
        (displayGlossWindow winTitle winSize)
        white
        (renderStepAnimate winSize durSecs dt sugCtx outRef stmStatsFlag)
        (const $ return ())

  return ()

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow winTitle winSize = GLO.InWindow winTitle winSize (0, 0)

modelToPicture :: (Int, Int)
               -> SugContext
               -> SimStepOut
               -> IO GLO.Picture
-- modelToPicture _ _ 
--  = return GLO.Blank 
modelToPicture winSize sugCtx (t, envCells, as) = do
  let env  = sugCtxEnv sugCtx
      dims = dimensionsDisc2d env
  return $ renderSugarScapeFrame winSize t dims envCells as

renderStep :: RandomGen g
           => Double
           -> DTime
           -> SugContext
           -> IORef (SimContext g)
           -> Bool
           -> ViewPort
           -> Float
           -> SimStepOut
           -> IO SimStepOut
renderStep durSecs dt sugCtx ssRef stmStatsFlag _ _ _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx stmStatsFlag
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx' 

  return out
   
renderStepAnimate :: RandomGen g
                  => (Int, Int)
                  -> Double
                  -> DTime
                  -> SugContext
                  -> IORef (SimContext g)
                  -> Bool
                  -> Float
                  -> IO GLO.Picture
renderStepAnimate winSize durSecs dt sugCtx ssRef stmStatsFlag _ = do
  simCtx <- readIORef ssRef
  (simCtx', out) <- simulationStep dt sugCtx simCtx stmStatsFlag
  writeIORef ssRef simCtx'

  _ <- checkTime durSecs simCtx'

  modelToPicture winSize sugCtx out 