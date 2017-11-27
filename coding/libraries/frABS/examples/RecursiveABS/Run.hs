module RecursiveABS.Run (
    runRecursiveABSSteps
  ) where

import RecursiveABS.Init

import FRP.FrABS

import System.IO

rngSeed = 42
agentCount = 2
dt = 1.0
time = 1
updateStrat = Sequential
shuffleAgents = True

-- TODO: repair, recursions not working yet

runRecursiveABSSteps :: IO ()
runRecursiveABSSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimNoEnv updateStrat shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createRecursiveABS agentCount
        
        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = simulateTime initAdefs initEnv params dt time
        let (_, as', _) = last ass

        putStrLn "Final Agents:"
        mapM_ (putStrLn . show . snd) as'