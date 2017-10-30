module AgentZero.Run (
    runAgentZeroWithRendering
  ) where

import           AgentZero.Environment
import           AgentZero.Init
import           AgentZero.Renderer 

import           FRP.FrABS

winSize = (800, 800)
winTitle = "Agent_Zero"
updateStrat = Sequential -- NOTE: agent-zero works BOTH for parallel and sequential, parallel is slower because collapsing the environments is a very expensive operation
envCollapsing = Just agentZeroEnvironmentsFold
envBeh = Just agentZeroEnvironmentBehaviour
shuffleAgents = True
rngSeed = 43
samplingTimeDelta = 1.0
frequency = 0

runAgentZeroWithRendering :: IO ()
runAgentZeroWithRendering =
    do
        params <- initSimulation updateStrat envBeh envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initAgentZeroEpstein

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderAgentZeroFrame
                            Nothing
