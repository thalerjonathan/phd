module Wildfire.Run (
    runWildfireWithRendering
  ) where

import Wildfire.Init
import Wildfire.Renderer as Renderer

import FRP.FrABS

import System.Random

winSize = (800, 800)
winTitle = "Wildfire"
updStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (35, 35)
samplingTimeDelta = 0.1
frequency = 0

runWildfireWithRendering :: IO ()
runWildfireWithRendering = 
    do
        params <- initSimulation updStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initWildfire envSize
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing
                            