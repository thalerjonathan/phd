module RecursiveABS.RecursiveABSRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import RecursiveABS.RecursiveABSModel
import RecursiveABS.RecursiveABSInit

import System.IO
import System.Random
import Control.Monad.Random

rngSeed = 42
agentCount = 10

runMetaABSStepsAndPrint :: IO ()
runMetaABSStepsAndPrint = do
                            hSetBuffering stdout NoBuffering
                            hSetBuffering stderr NoBuffering
                            initRng rngSeed
                            (as, env) <- createMetaABSAgentsAndEnv agentCount
                            params <- simParams

                            putStrLn "Initial Agents:"
                            mapM_ (putStrLn . show . adState) as

                            let steps = 1
                            let ass = processSteps as env params 1.0 steps
                            let (as', _) = last ass

                            putStrLn "Final Agents:"
                            mapM (putStrLn . show . aoState) as'

                            return ()

simParams :: IO (SimulationParams MetaABSEnvCell ())
simParams = 
    do
        rng <- getSplit
        return SimulationParams {
            simStrategy = Sequential,
            simEnvCollapse = Nothing,
            simShuffleAgents = True,
            simRng = rng
        }

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g