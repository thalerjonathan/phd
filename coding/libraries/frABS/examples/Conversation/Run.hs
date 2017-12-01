module Conversation.Run (
    runConversationSteps
  ) where

import Conversation.Init

import FRP.FrABS

import System.IO

rngSeed = 42
agentCount = 2
dt = 1.0
t = 2
updateStrat = Sequential
shuffleAgents = True

-- TODO: repair, conversations not yet working

runConversationSteps :: IO ()
runConversationSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimNoEnv updateStrat shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createConversation agentCount
        
        putStrLn "Initial Agents:"
        mapM_ (putStrLn . show . adState) initAdefs

        let ass = simulateTime initAdefs initEnv params dt t
        let (_, as', _) = last ass

        putStrLn "Final Agents:"
        mapM_ (putStrLn . show . snd) as'