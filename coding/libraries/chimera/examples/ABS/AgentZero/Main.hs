module Main
  (
    main
  ) where

import Control.Monad.Random
import FRP.Chimera
import FRP.Yampa

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

shuffleAgents :: Bool
shuffleAgents = True

rngSeed :: Int
rngSeed = 43

dt :: DTime
dt = 1.0

frequency :: Int
frequency = 0

main :: IO ()
main = do
  initRng $ Just rngSeed
  g <- getSplit
  let envBeh = Just $ agentZeroEnvironmentBehaviour g

  params <- initSimulation updateStrat envBeh envFolding shuffleAgents Nothing
  (initAdefs, initEnv) <- evalRandIO $ initAgentZeroEpstein

  simulateAndRender 
    initAdefs
    initEnv
    params
    dt
    frequency
    winTitle
    winSize
    renderAgentZeroFrame
    Nothing