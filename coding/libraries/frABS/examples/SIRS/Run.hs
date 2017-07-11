module SIRS.Run (
    runSIRSWithRendering,
    runSIRSStepsAndRender
  )  where

import SIRS.Init
import SIRS.Model
import SIRS.Renderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.Init
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
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSIRS cells initialInfectionProb

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
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSIRS cells initialInfectionProb

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

