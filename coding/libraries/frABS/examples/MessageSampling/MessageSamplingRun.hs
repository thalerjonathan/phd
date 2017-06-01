module MessageSampling.MessageSamplingRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import MessageSampling.MessageSamplingInit
import MessageSampling.MessageSamplingModel

import System.IO
import System.Random
import Control.Monad.Random

rngSeed = 42
agentCount = 2

runMessageSamplingAndPrint :: IO ()
runMessageSamplingAndPrint = do
                                hSetBuffering stdout NoBuffering
                                hSetBuffering stderr NoBuffering
                                initRng rngSeed
                                (as, env) <- createMessageSamplingAgentsAndEnv agentCount
                                params <- simParams

                                putStrLn "Initial Agents:"
                                mapM_ (putStrLn . show . adState) as

                                let steps = 2
                                let ass = processSteps as env params 1.0 steps
                                let (as', _) = last ass

                                putStrLn "Final Agents:"
                                mapM (putStrLn . show . aoState) as'

                                return ()

simParams :: IO (SimulationParams MessageSamplingEnvCell ())
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