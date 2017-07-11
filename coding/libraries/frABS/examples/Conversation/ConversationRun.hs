module Conversation.ConversationRun (
    runConversationSteps
  ) where

import Conversation.ConversationInit

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Init

import System.IO

rngSeed = 42
agentCount = 2
samplingTimeDelta = 1.0
steps = 2
updateStrat = Sequential
envCollapsing = Nothing
shuffleAgents = True

runConversationSteps :: IO ()
runConversationSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createConversation agentCount
        
        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = processSteps initAdefs initEnv params samplingTimeDelta steps
        let (as', _) = last ass

        putStrLn "Final Agents:"
        mapM (putStrLn . show . aoState) as'

        return ()