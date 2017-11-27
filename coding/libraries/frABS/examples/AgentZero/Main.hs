module Main
  (
    runAgentZeroWithRendering
  ) where

import FRP.FrABS

import Environment
import Init
import Renderer 

winSize :: (Int, Int)
winSize = (800, 800)

winTitle :: String
winTitle = "Agent_Zero"

updateStrat :: UpdateStrategy
updateStrat = Sequential -- NOTE: agent-zero works BOTH for parallel and sequential, parallel is slower because collapsing the environments is a very expensive operation

envFolding :: Maybe AgentZeroEnvironmentFolding
envFolding = Just agentZeroEnvironmentsFold

envBeh :: Maybe AgentZeroEnvironmentBehaviour
envBeh = Just agentZeroEnvironmentBehaviour

shuffleAgents :: Bool
shuffleAgents = True

rngSeed :: Int
rngSeed = 43

dt = DTime
dt = 1.0

frequency :: Int
frequency = 0

runAgentZeroWithRendering :: IO ()
runAgentZeroWithRendering = do
  params <- initSimulation updateStrat envBeh envFolding shuffleAgents (Just rngSeed)
  (initAdefs, initEnv) <- evalRandIO $ initAgentZeroEpstein

  simulateAndRender 
    initAdefs
    initEnv
    params
    samplingTimeDelta
    frequency
    winTitle
    winSize
    renderAgentZeroFrame
    Nothing
