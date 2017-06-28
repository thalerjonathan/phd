module FrSIRS.FrSIRSRun where

import FrSIRS.FrSIRSInit
import FrSIRS.FrSIRSModel
import FrSIRS.FrSIRSRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

winSize = (800, 800)
winTitle = "FrSIRS"
updateStrat = Parallel
shuffleAgents = False
rngSeed = 42
samplingTimeDelta = 0.5
frequency = 0
cells = (51, 51)
steps = 60

runFrSIRSWithRendering :: IO ()
runFrSIRSWithRendering =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRS cells initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing

runFrSIRSStepsAndRender :: IO ()
runFrSIRSStepsAndRender =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRS cells initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

