module RecursiveABS.RecursiveABSRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Utils

import RecursiveABS.RecursiveABSInit

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
        hSetBuffering stderr NoBuffering

        initRng rngSeed

        (initAdefs, initEnv) <- createMetaABSAgentsAndEnv agentCount
        params <- initSimParams updateStrat envCollapsing shuffleAgents

        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = processSteps initAdefs initEnv params samplingTimeDelta steps
        let (as', _) = last ass

        putStrLn "Final Agents:"
        mapM (putStrLn . show . aoState) as'

        return ()