module SugarScape.Run (
    runSugarScapeWithRendering
  ) where

import SugarScape.Init
import SugarScape.Renderer
import SugarScape.Environment

import FRP.FrABS

winSize = (800, 800)
winTitle = "SugarScape"
rngSeed = 42
agentCount = 400
envSize = (50, 50)
updateStrat = Sequential    -- Sugarscape works ONLY with Sequential AND must be shuffled
envCollapsing = Nothing
envBeh = Just sugarScapeEnvironmentBehaviour
shuffleAgents = True        -- Sugarscape works ONLY with Sequential AND must be shuffled
samplingTimeDelta = 1.0
frequency = 0

runSugarScapeWithRendering :: IO ()
runSugarScapeWithRendering = 
    do
        params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSugarScape agentCount envSize params
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderSugarScapeFrame
                            Nothing