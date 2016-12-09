module Main where

import System.Random
import System.Environment (getArgs, getProgName)

import Utils
import HACAgent as Agent
import qualified HACFrontend as Front
import qualified HACSimulation as Sim
import qualified HACSimulationImpl as SimImpl

-----------------------------------------------------------------------------------------------------------------------
-- IO-Driven Simulation
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    let dt = 0.01
    let wt = Border -- Infinite | Border | Wraping | InfiniteWraping
    let agentCount = 60
    let heroDist = 0.25
    let rngSeed = 42
    let rng = mkStdGen rngSeed
    let (agents, rng') = Agent.createRandAgentStates rng agentCount heroDist
    let simIn = Sim.SimIn { Sim.simInInitAgents = agents, Sim.simInWorldType = wt }
    let steps = 1000
    Front.initialize
    SimImpl.simulationIO simIn (render dt wt)
    Front.shutdown
-}
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Step-Driven Simulation
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    let dt = 0.01
    let wt = Border -- Infinite | Border | Wraping | InfiniteWraping
    let agentCount = 60
    let heroDist = 0.25
    let rngSeed = 42
    let rng = mkStdGen rngSeed
    let (agents, rng') = Agent.createRandAgentStates rng agentCount heroDist
    let simIn = Sim.SimIn { Sim.simInInitAgents = agents, Sim.simInWorldType = wt }
    let steps = 1000
    Front.initialize
    -- NOTE: this won't lead to "long numbercrunching" when stepCount is high because of haskells lazyness. Just an
    --       unevaluated array will be returned and then when rendering the steps the required list-element will be
    --       calculated by the simulation.
    let outs = SimImpl.simulationStep simIn dt steps
    freezeRender wt (last outs)
    Front.shutdown
-}

main :: IO ()
main = do
    let dt = 0.01
    let wt = Border -- Infinite | Border | Wraping | InfiniteWraping
    let agents = createTestAgents
    let simIn = Sim.SimIn { Sim.simInInitAgents = agents, Sim.simInWorldType = wt }
    let steps = 20
    Front.initialize
    -- NOTE: this won't lead to "long numbercrunching" when stepCount is high because of haskells lazyness. Just an
    --       unevaluated array will be returned and then when rendering the steps the required list-element will be
    --       calculated by the simulation.
    let outs = SimImpl.simulationStep simIn dt steps
    freezeRender wt (last outs)
    Front.shutdown

-----------------------------------------------------------------------------------------------------------------------

render :: Double -> Agent.WorldType -> Sim.SimOut -> IO (Bool, Double)
render dt wt simOut = do
    let aos = Sim.simOutAgents simOut
    winOpen <- Front.renderFrame aos wt
    return (winOpen, dt)

-- NOTE: used to freeze a given output: render it until the window is closed
freezeRender :: Agent.WorldType -> Sim.SimOut -> IO (Bool, Double)
freezeRender wt simOut = do
    (cont, _) <- render 0.0 wt simOut
    if cont then
        freezeRender wt simOut
        else
            return (False, 0.0)

renderOutputs :: [Sim.SimOut] -> Agent.WorldType -> IO (Bool, Double)
renderOutputs (s:xs) wt
    | null xs = return (True, 0.0)
    | otherwise = do
        (cont, dt) <- render 0.0 wt s
        if cont then
            renderOutputs xs wt
                else
                    return (True, 0.0)

-----------------------------------------------------------------------------------------------------------------------
-- Testing Rendering --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    let dt = 0.5
    let as = createTestAgents
    let aos' = createTestAgentOuts
    Front.initialize
    let wt = Border
    let simIn = Sim.SimIn { Sim.simInInitAgents = as, Sim.simInWorldType = wt }
    let outs = SimImpl.simulationStep simIn dt 1
    freezeRender wt (head outs)
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
                            hero = False }

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
