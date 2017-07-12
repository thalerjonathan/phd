module PrisonersDilemma.Run (
    runPDWithRendering,
    runPDSteps
  ) where

import PrisonersDilemma.Init
import PrisonersDilemma.Model
import PrisonersDilemma.Renderer as Renderer

import FRP.FrABS

winSize = (800, 800)
winTitle = "Prisoners Dilemma"
updateStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (29, 29)
samplingTimeDelta = 0.3
frequency = 0
steps = 100

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

runPDSteps :: IO ()
runPDSteps = 
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initPrisonersDilemma envSize
        
        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let finalAs = fst $ last asenv
        mapM_ (\ao -> putStrLn $ (show $ pdCurrAction $ aoState ao)) finalAs