module RecursiveABS.Run (
    runRecursiveABSSteps
  ) where

import RecursiveABS.Init

import FRP.FrABS

import System.IO

rngSeed = 42
agentCount = 2
samplingTimeDelta = 1.0
steps = 1
updateStrat = Sequential
shuffleAgents = True

runRecursiveABSSteps :: IO ()
runRecursiveABSSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimNoEnv updateStrat shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createRecursiveABS agentCount
        
        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = processSteps initAdefs initEnv params samplingTimeDelta steps
        let (as', _) = last ass

        putStrLn "Final Agents:"
        mapM (putStrLn . show . aoState) as'

        return ()