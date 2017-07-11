module PrisonersDilemma.PDRun (
    runPDWithRendering
  ) where

import PrisonersDilemma.PDInit
import PrisonersDilemma.PDRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.Init
import FrABS.Rendering.GlossSimulator

import System.Random

winSize = (800, 800)
winTitle = "Prisoners Dilemma"
updateStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (49, 49)
samplingTimeDelta = 0.2
frequency = 0

runPDWithRendering :: IO ()
runPDWithRendering = 
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initPrisonersDilemma envSize
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing
