module SugarScape.Run (
    runSugarScapeWithRendering,
    runSugarScapeStepsAndRender,

    runSugarScapeStepsAndExport
  ) where

import SugarScape.Init
import SugarScape.Renderer
import SugarScape.Environment
import SugarScape.Exporter

import FRP.FrABS

import SugarScape.Common
import SugarScape.Model

winSize = (800, 800)
winTitle = "SugarScape"
rngSeed = 42
agentCount = 400
envSize = (50, 50)
updateStrat = Sequential    -- Sugarscape works ONLY with Sequential AND must be shuffled
shuffleAgents = True        -- Sugarscape works ONLY with Sequential AND must be shuffled
envCollapsing = Nothing
envBeh = Just sugarScapeEnvironmentBehaviour
samplingTimeDelta = 1.0
frequency = 0
steps = 500

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

runSugarScapeStepsAndRender :: IO ()
runSugarScapeStepsAndRender = 
    do
        params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSugarScape agentCount envSize params
        
        simulateStepsAndRender initAdefs
                                initEnv
                                params
                                samplingTimeDelta
                                steps
                                winTitle
                                winSize
                                renderSugarScapeFrame

runSugarScapeStepsAndExport :: IO ()
runSugarScapeStepsAndExport = 
    do
        params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSugarScape agentCount envSize params
        
        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        writeSugarscapeDynamics asenv