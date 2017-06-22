module SIRS.SIRSRun where

import SIRS.SIRSInit
import SIRS.SIRSModel
import SIRS.SIRSRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

import System.Random

winSize = (800, 800)
winTitle = "SIRS"
updateStrat = Sequential
shuffleAgents = True
rngSeed = 42
samplingTimeDelta = 1.0
frequency = 0
cells = (31, 31)
steps = 10

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
    do
        initRng rngSeed
        initAdefs <- createRandomSIRSAgents cells initialInfectionProb
        initEnv <- createSIRSEnv cells initAdefs
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender =
    do
        initRng rngSeed
        initAdefs <- createRandomSIRSAgents cells initialInfectionProb
        initEnv <- createSIRSEnv cells initAdefs
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

