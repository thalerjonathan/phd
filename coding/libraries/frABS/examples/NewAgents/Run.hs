module NewAgents.Run (
    runNewAgentsSteps
  ) where

import NewAgents.Model
import NewAgents.Init

import FRP.FrABS

import Text.Printf

import System.IO

rngSeed = 42
samplingTimeDelta = 1.0
agentCount = 1
steps = 1
updateStrat = Parallel -- NOTE: would not work correctly when using Sequential traversion
shuffleAgents = False

runNewAgentsSteps :: IO ()
runNewAgentsSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initNewAgents agentCount
        
        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps

        let (asFinal, envFinal) = last asenv
        mapM printNewAgent asFinal

        return ()

printNewAgent :: NewAgentOut -> IO ()
printNewAgent ao =
    do
        let aid = aoId ao
        let s = value $ aoState ao
        putStrLn $ "Agent " ++ show aid ++ ": state = " ++ show s