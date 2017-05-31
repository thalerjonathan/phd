module RecursiveABS.RecursiveABSRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import RecursiveABS.RecursiveABSInit

import System.IO
import System.Random

rngSeed = 42
agentCount = 10
parallelStrategy = Nothing

runMetaABSStepsAndPrint :: IO ()
runMetaABSStepsAndPrint = do
                            hSetBuffering stdout NoBuffering
                            hSetBuffering stderr NoBuffering
                            initRng rngSeed
                            (as, env) <- createMetaABSAgentsAndEnv agentCount

                            putStrLn "Initial Agents:"
                            mapM_ (putStrLn . show . adState) as

                            let steps = 1
                            let ass = processSteps as env parallelStrategy 1.0 steps
                            let (as', _) = last ass

                            putStrLn "Final Agents:"
                            mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g