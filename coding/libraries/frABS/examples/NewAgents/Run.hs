module NewAgents.Run (
    runNewAgentsSteps,

    runNewAgentsDebug
  ) where

import NewAgents.Model
import NewAgents.Init

import FRP.FrABS

import Text.Printf

import System.IO

rngSeed = 42
samplingTimeDelta = 1.0
agentCount = 1
steps = 10
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

runNewAgentsDebug :: IO ()
runNewAgentsDebug = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initNewAgents agentCount
        
        processDebug initAdefs initEnv params samplingTimeDelta renderFunc

    where
        renderFunc :: Bool -> ([NewAgentObservable], NewAgentEnvironment) -> IO Bool
        renderFunc _ (aobs, env) = mapM_ printNewAgent aobs >> (return False)

printNewAgent :: NewAgentObservable -> IO ()
printNewAgent (aid, s) = putStrLn $ "Agent " ++ show aid ++ ": state = " ++ show s