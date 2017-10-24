module NewAgents.Run (
    runNewAgentsSteps,

    runNewAgentsDebug
  ) where

import FRP.Yampa

import NewAgents.Model
import NewAgents.Init

import FRP.FrABS

import System.IO

rngSeed = 42
dt = 0.2
agentCount = 1
t = 11
updateStrat = Parallel -- NOTE: would not work correctly when using Sequential traversion
shuffleAgents = False

runNewAgentsSteps :: IO ()
runNewAgentsSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initNewAgents agentCount
        
        let asenv = simulateTime initAdefs initEnv params dt t

        let (t, asFinal, _) = last asenv
        mapM_ printNewAgent asFinal
        print t

runNewAgentsDebug :: IO ()
runNewAgentsDebug = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initNewAgents agentCount
        
        simulateDebug initAdefs initEnv params dt renderFunc

    where
        renderFunc :: Bool -> (Time, [NewAgentObservable], NewAgentEnvironment) -> IO Bool
        renderFunc _ (_, aobs, _) = mapM_ printNewAgent aobs >> (return False)

printNewAgent :: NewAgentObservable -> IO ()
printNewAgent (aid, s) = putStrLn $ "Agent " ++ show aid ++ ": state = " ++ show s