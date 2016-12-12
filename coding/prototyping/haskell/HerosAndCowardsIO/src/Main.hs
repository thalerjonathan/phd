module Main where

import System.Random
import System.Environment (getArgs, getProgName)

import HACAgent as Agent
import qualified HACFrontend as Front
import qualified HACSimulation as Sim
import qualified HACSimulationImpl as SimImpl

import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

-----------------------------------------------------------------------------------------------------------------------
-- IO-Driven Simulation
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
    GLO.simulateIO Front.display
        GLO.white
        30                                      -- Number of simulation steps to take for each second of real time.
        simIn                                   -- The initial model.
        (\model -> Front.renderFrame model)            -- A function to convert the model to a picture.
        (\viewport dtRendering model ->  -- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds
            do
                outs <- SimImpl.simulationStep model dt 1
                let out = head outs
                let agentOuts = Sim.simOutAgents out
                let agentStates = map agentOutState agentOuts
                let model' = Sim.SimIn { Sim.simInInitAgents = agentStates, Sim.simInWorldType = wt }
                return model' )

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
     -}
-----------------------------------------------------------------------------------------------------------------------