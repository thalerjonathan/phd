module PrisonersDilemma.Run (
    runPDWithRendering,
    runPDStepsAndRender,

    runPDSteps
  ) where

import PrisonersDilemma.Init
import PrisonersDilemma.Model
import PrisonersDilemma.Renderer

import FRP.FrABS

winSize = (800, 800)
winTitle = "Prisoners Dilemma"
updateStrat = Parallel
shuffleAgents = False
rngSeed = 42
envSize = (59, 59)
dt = 0.2
frequency = 0
t = 545 * dt

-- TODO: repair

runPDWithRendering :: IO ()
runPDWithRendering = 
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initPrisonersDilemma envSize
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            dt
                            frequency
                            winTitle
                            winSize
                            renderPDFrame
                            Nothing

runPDStepsAndRender :: IO ()
runPDStepsAndRender = 
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initPrisonersDilemma envSize
        
        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            dt
                            t
                            winTitle
                            winSize
                            renderPDFrame

runPDSteps :: IO ()
runPDSteps = 
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initPrisonersDilemma envSize
        
        let asenv = simulateTime initAdefs initEnv params dt t
        let (_, finalAs, _) = last asenv
        mapM_ (putStrLn . (show . pdCurrAction . snd))finalAs