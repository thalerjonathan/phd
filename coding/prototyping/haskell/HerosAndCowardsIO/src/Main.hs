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
{-
main :: IO ()
main = do
    let dt = 0.01
    let wt = Border -- Infinite | Border | Wraping | InfiniteWraping
    let agentCount = 1000
    let heroDist = 0.25
    let rngSeed = 42
    setStdGen $ mkStdGen rngSeed
    agents <- Agent.createRandAgentStates agentCount heroDist
    let simIn = Sim.SimIn { Sim.simInInitAgents = agents, Sim.simInWorldType = wt }
    Front.initialize
    SimImpl.simulationIO simIn (render dt wt)
    Front.shutdown
    -}
-----------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------------
-- Step-Driven Simulation
-----------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    let dt = 0.01
    let wt = Border -- Infinite | Border | Wraping | InfiniteWraping
    let agentCount = 1000
    let heroDist = 0.25
    let rngSeed = 42
    setStdGen $ mkStdGen rngSeed
    agents <- Agent.createRandAgentStates agentCount heroDist
    let simIn = Sim.SimIn { Sim.simInInitAgents = agents, Sim.simInWorldType = wt }
    let steps = 1000
    Front.initialize
    -- NOTE: this won't lead to "long numbercrunching" when stepCount is high because of haskells lazyness. Just an
    --       unevaluated array will be returned and then when rendering the steps the required list-element will be
    --       calculated by the simulation.
    outs <- SimImpl.simulationStep simIn dt steps
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