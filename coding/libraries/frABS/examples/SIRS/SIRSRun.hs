module SIRS.SIRSRun where

import SIRS.SIRSInit
import SIRS.SIRSModel
import SIRS.SIRSRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.Utils
import FrABS.Rendering.GlossSimulator

import System.Random

winSize = (800, 800)
winTitle = "SIRS"
updateStrat = Parallel
shuffleAgents = True
rngSeed = 42
samplingTimeDelta = 0.5
frequency = 0
cells = (31, 31)
steps = 60

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
    do
        initRng rngSeed
        (initAdefs, initEnv) <- createSIRS cells initialInfectionProb
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

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender =
    do
        initRng rngSeed
        (initAdefs, initEnv) <- createSIRS cells initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

