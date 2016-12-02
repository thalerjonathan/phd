module Main where

import System.Random
import System.Environment (getArgs, getProgName)

import HACAgent as Agent
import qualified HACFrontend as Front
import qualified HACSimulation as Sim
import qualified HACSimulationImpl as SimImpl

-----------------------------------------------------------------------------------------------------------------------
-- IO-Driven Simulation
-----------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    let agentCount = 300
    let heroDist = 0.5
    let dt = 1.0
    let rngSeed = 42
    let rng = mkStdGen rngSeed
    let agents = Agent.createRandAgentStates rng agentCount heroDist
    Front.initialize
    SimImpl.simulationIO agents (output dt)
    Front.shutdown
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Step-Driven Simulation
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    let agentCount = 60
    let heroDist = 0.5
    let dt = 1.0
    let rngSeed = 42
    let rng = mkStdGen rngSeed
    let stepCount = 100
    let agents = Agent.createRandAgentStates rng agentCount heroDist
    Front.initialize
    -- NOTE: this won't lead to "long numbercrunching" when stepCount is high because of haskells lazyness. Just an
    --       unevaluated array will be returned and then when rendering the steps the required list-element will be
    --       calculated by the simulation.
    let outs = SimImpl.simulationStep agents dt stepCount
    renderSteps outs
    Front.shutdown

renderSteps :: [Sim.SimOut] -> IO (Bool, Double)
renderSteps (s:xs)
    | null xs = return (True, 0.0)
    | otherwise = do
        (cont, dt) <- output 0.0 s
        if cont then
            renderSteps xs
                else
                    return (True, 0.0)
-}
-----------------------------------------------------------------------------------------------------------------------

parseArgs :: IO Sim.SimIn
parseArgs = do
    return Sim.SimIn { Sim.simInAllAgents = [] }

output :: Double -> Sim.SimOut -> IO (Bool, Double)
output dt simOut = do
    let aos = Sim.simOutAllAgents simOut
    winOpen <- Front.renderFrame aos
    return (winOpen, dt)

-----------------------------------------------------------------------------------------------------------------------
-- Testing Rendering --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    Front.initialize
    let dt = 0.5
    let as = createTestAgents
    let aos' = createTestAgentOuts
    let aos = Sim.simulationStep as dt 1
    --testRender aos
    Sim.simulationIO as (output dt)
    Front.shutdown
-}

createTestAgents :: [Agent.AgentState]
createTestAgents = [a1, a2, a3]
    where
        a1 = Agent.AgentState { agentId = 0,
                             agentPos = (0.5, 0.25),
                             enemy = 2,
                             friend = 1,
                             hero = True }

        a2 = Agent.AgentState { agentId = 1,
                            agentPos = (0.75, 0.75),
                            enemy = 2,
                            friend = 0,
                            hero = True }

        a3 = Agent.AgentState { agentId = 2,
                            agentPos = (0.25, 0.75),
                            enemy = 1,
                            friend = 0,
                            hero = True }

createTestAgentOuts :: [Agent.AgentOut]
createTestAgentOuts = [ao1, ao2, ao3]
    where
        ao1 = Agent.AgentOut{ agentOutState = Agent.AgentState { agentId = 0,
                                                     agentPos = (0.25, 0.5),
                                                     enemy = 1,
                                                     friend = 2,
                                                     hero = True},
                         agentOutDir = (-1.0, 0.0) }

        ao2 = Agent.AgentOut{ agentOutState = Agent.AgentState { agentId = 1,
                                                      agentPos = (0.2, 0.2),
                                                      enemy = 0,
                                                      friend = 2,
                                                      hero = True},
                          agentOutDir = (1.0, 0.0) }

        ao3 = Agent.AgentOut{ agentOutState = Agent.AgentState { agentId = 2,
                                                      agentPos = (0.3, 0.3),
                                                      enemy = 0,
                                                      friend = 1,
                                                      hero = True},
                          agentOutDir = (1.0, 0.0) }

-- NOTE: used to freeze a given output: render it until the window is closed
testRender :: [Agent.AgentOut] -> IO ()
testRender aos = do
    continue <- Front.renderFrame aos
    if continue then
        testRender aos
        else
            return ()