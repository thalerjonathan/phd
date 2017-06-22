module AgentZero.AgentZeroRun where

import AgentZero.AgentZeroInit
import AgentZero.AgentZeroEnvironment
import AgentZero.AgentZeroRenderer as Renderer

import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

import System.Random

winSize = (800, 800)
winTitle = "Agent_Zero"
updateStrat = Sequential -- NOTE: agent-zero works BOTH for parallel and sequential, parallel is slower because collapsing the environments is a very expensive operation 
envCollapsing = Just agentZeroEnvironmentsCollapse
shuffleAgents = True
rngSeed = 42
samplingTimeDelta = 1.0
frequency = 0

runAgentZeroWithRendering :: IO ()
runAgentZeroWithRendering = 
    do
        initRng rngSeed
        (initAdefs, initEnv) <- initAgentZeroEpstein
        params <- initSimParams updateStrat envCollapsing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing