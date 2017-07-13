module Wildfire.Run (
    runWildfireWithRendering
  ) where

import Wildfire.Init
import Wildfire.Renderer

import FRP.FrABS

winSize = (800, 800)
winTitle = "Wildfire"
updStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (59, 59)
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
                            renderWildfireFrame
                            Nothing
                            