module SugarScape.GlossRunner
  ( runGloss
  ) where

import            Data.IORef

import            Control.Monad.Random
import qualified  Graphics.Gloss as GLO
import            Graphics.Gloss.Interface.IO.Animate
import            Graphics.Gloss.Interface.IO.Simulate

import            SugarScape.Model
import            SugarScape.Renderer
import            SugarScape.Simulation

import Debug.Trace

runGloss :: RandomGen g
         => SimulationState g
         -> SimStepOut
         -> Int
         -> AgentVis
         -> SiteVis
         -> IO ()
runGloss initSimState initOut stepsPerSec av cv = do
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

  let (_, _, _, aos) = out
      agentWealths   = map (sugObsSugLvl . snd) aos
      gini           = giniCoeff agentWealths

  trace ("Gini Coeff = " ++ show gini) (modelToPicture winSize av cv out )

giniCoeff :: [Double]
          -> Double
giniCoeff xs = numer / denom
  where
    n = fromIntegral $ length xs
    
    numer = sum [abs (x_i - x_j) | x_i <- xs, x_j <- xs] 
    denom = 2 * n * sum xs