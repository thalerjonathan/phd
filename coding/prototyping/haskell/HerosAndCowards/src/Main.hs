module Main where

import System.Random
import System.Environment (getArgs, getProgName)

import FRP.Yampa

import HACAgent as Agent
import HACSimulation as Sim
import HACFrontend as Front

import HACYampaBackend as YampaBack
import HACClassicBackend as ClassicBack

heroDistribution :: Double
heroDistribution = 1.0

-----------------------------------------------------------------------------------------------------------------------
-- Testing Rendering --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    Front.initialize
    let aos = createTestAos
    testRender aos
    Front.shutdown

createTestAos :: [AgentOut]
createTestAos = [ao1, ao2, ao3]
    where
        ao1 = AgentOut{ agentOutState = AgentState { agentId = 0,
                                                     agentPos = (0.1, 0.1),
                                                     enemy = 1,
                                                     friend = 2,
                                                     hero = True},
                         agentOutDir = (-1.0, 0.0) }

        ao2 = AgentOut{ agentOutState = AgentState { agentId = 1,
                                                      agentPos = (0.2, 0.2),
                                                      enemy = 0,
                                                      friend = 2,
                                                      hero = True},
                          agentOutDir = (1.0, 0.0) }

        ao3 = AgentOut{ agentOutState = AgentState { agentId = 2,
                                                      agentPos = (0.3, 0.3),
                                                      enemy = 0,
                                                      friend = 1,
                                                      hero = True},
                          agentOutDir = (1.0, 0.0) }

testRender :: [AgentOut] -> IO ()
testRender aos = do
    continue <- Front.renderFrame aos
    if continue then
        testRender aos
        else
            return ()
-}
-----------------------------------------------------------------------------------------------------------------------
-- CLASSIC/MONADIC --
-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    -- g <- getStdGen
    let g = mkStdGen 42 -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g 5 heroDistribution
    ClassicBack.process as output
    --let as' = ClassicBack.process_ as 100000
    --Front.renderFrame (map agentPos as')
    Front.shutdown

output :: [AgentOut] -> IO (Bool, Double)
output aos = do
    winClosed <- Front.renderFrame aos
    return (winClosed, 0.5)

-----------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------
-- YAMPA --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    -- g <- getStdGen
    let g = mkStdGen 42 -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g 5 heroDistribution
    reactimate Main.initialize input output (YampaBack.process as)
    Front.shutdown

initialize :: IO Sim.SimIn
initialize = do
    return Sim.SimIn {}

input :: Bool -> IO (DTime, Maybe Sim.SimIn)
input _ = return (0.5, Nothing)

output :: Bool -> Sim.SimOut -> IO Bool
output _ out = do
    let as = Sim.simOutAllAgents out
    winOpened <- Front.renderFrame as
    return $ not winOpened
-}
-----------------------------------------------------------------------------------------------------------------------