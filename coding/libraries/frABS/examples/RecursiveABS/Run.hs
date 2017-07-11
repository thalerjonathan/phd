module RecursiveABS.Run (
    runRecursiveABSSteps
  ) where

import RecursiveABS.Init

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Init

import System.IO

rngSeed = 42
agentCount = 10
samplingTimeDelta = 1.0
steps = 1
updateStrat = Sequential
envCollapsing = Nothing
shuffleAgents = True

runRecursiveABSSteps :: IO ()
runRecursiveABSSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createRecursiveABS agentCount
        
        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = processSteps initAdefs initEnv params samplingTimeDelta steps
        let (as', _) = last ass

        putStrLn "Final Agents:"
        mapM (putStrLn . show . aoState) as'

        return ()