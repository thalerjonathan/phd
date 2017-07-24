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

import SugarScape.AgentCommon
import SugarScape.Model

winSize = (800, 800)
winTitle = "SugarScape"
rngSeed = 42
agentCount = 550
envSize = (50, 50)
updateStrat = Sequential    -- Sugarscape works ONLY with Sequential AND must be shuffled
shuffleAgents = True        -- Sugarscape works ONLY with Sequential AND must be shuffled
envCollapsing = Nothing
envBeh = Just sugarScapeEnvironmentBehaviour
samplingTimeDelta = 1.0
frequency = 0
steps = 1

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

{-
testBestCells :: IO ()
testBestCells =
    do
        let protoCell = SugarScapeEnvCell {
            sugEnvSugarCapacity = 0,
            sugEnvSugarLevel = 0,

            sugEnvSpiceCapacity = 0,
            sugEnvSpiceLevel = 0,

            sugEnvPolutionLevel = 0,
            sugEnvOccupier = Nothing
        }

        let cells = [((1,10), protoCell { sugEnvSugarLevel = 3 }),
                     ((0,1), protoCell { sugEnvSugarLevel = 3 }),
                     ((2,1), protoCell { sugEnvSugarLevel = 2 }),
                     ((1,2), protoCell { sugEnvSugarLevel = 2 })]

        let bc = selectBestCells bestMeasureSugarLevel (1,1) cells
        mapM_ showCell bc

    where
        showCell :: (Discrete2dCoord, SugarScapeEnvCell) -> IO ()
        showCell (coord, c) =
            do
                putStrLn ((show coord) ++ ": " ++ (show $ sugEnvSugarLevel c))
-}