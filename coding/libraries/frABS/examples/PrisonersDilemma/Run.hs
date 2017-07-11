module PrisonersDilemma.Run (
    runPDWithRendering
  ) where

import PrisonersDilemma.Init
import PrisonersDilemma.Renderer as Renderer

import FRP.FrABS

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
