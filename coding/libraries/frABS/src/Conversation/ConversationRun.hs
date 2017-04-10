module Conversation.ConversationRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import Conversation.ConversationInit

import System.IO
import System.Random

rngSeed = 42
agentCount = 2
parallelStrategyFlag = False

runConversationStepsAndPrint :: IO ()
runConversationStepsAndPrint = do
                                hSetBuffering stdout NoBuffering
                                hSetBuffering stderr NoBuffering
                                initRng rngSeed
                                (as, env) <- createConversationAgentsAndEnv agentCount

                                putStrLn "Initial Agents:"
                                mapM_ (putStrLn . show . adState) as

                                let steps = 1
                                let ass = processSteps as env parallelStrategyFlag 1.0 steps
                                let (as', _) = last ass

                                putStrLn "Final Agents:"
                                mapM (putStrLn . show . aoState) as'

                                return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g