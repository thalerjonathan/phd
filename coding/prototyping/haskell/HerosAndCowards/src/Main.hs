module Main where

import Data.IORef
import System.Random
import System.Environment (getArgs, getProgName)

import FRP.Yampa

import HACAgent as Agent
import HACSimulation as Sim
import HACFrontend as Front

import HACYampaBackend as YampaBack
import HACClassicBackend as ClassicBack

heroDistribution :: Double
heroDistribution = 0.5

agentCount :: Int
agentCount = 2000

rngSeed :: Int
rngSeed = 42

timeStep :: Double
timeStep = 1.0

-----------------------------------------------------------------------------------------------------------------------
-- CLASSIC/MONADIC --
-----------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    -- g <- getStdGen
    let g = mkStdGen rngSeed -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g agentCount heroDistribution
    ClassicBack.process as output
    --let as' = ClassicBack.process_ as 100000
    --Front.renderFrame (map agentPos as')
    Front.shutdown

output :: [AgentOut] -> IO (Bool, Double)
output aos = do
    -- Agent.showAgents (map agentOutState aos)
    winClosed <- Front.renderFrame aos
    return (winClosed, timeStep)

-----------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------
-- YAMPA --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    let g = mkStdGen rngSeed -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createRandAgentStates g agentCount heroDistribution
    reactimate (Main.init as) input output (YampaBack.process as)
    Front.shutdown

init :: [Agent.AgentState] -> IO [Agent.AgentState]
init initAs = do
    return initAs

input :: Bool -> IO (DTime, Maybe [Agent.AgentState])
input _ = do
    return (timeStep, Nothing)

output :: Bool -> [Agent.AgentOut] -> IO Bool
output _ aos = do
    winOpened <- Front.renderFrame aos
    return $ not winOpened
    -}    
----------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------
-- Testing Rendering --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    Front.initialize
    let as = createTestAgents
    let aos' = createTestAgentOuts
    let aos = ClassicBack.process_ as 1
    --testRender aos
    ClassicBack.process as output
    Front.shutdown
-}

createTestAgents :: [AgentState]
createTestAgents = [a1, a2, a3]
    where
        a1 = AgentState { agentId = 0,
                             agentPos = (0.5, 0.25),
                             enemy = 2,
                             friend = 1,
                             hero = True }

        a2 = AgentState { agentId = 1,
                            agentPos = (0.75, 0.75),
                            enemy = 2,
                            friend = 0,
                            hero = True }

        a3 = AgentState { agentId = 2,
                            agentPos = (0.25, 0.75),
                            enemy = 1,
                            friend = 0,
                            hero = True }

createTestAgentOuts :: [AgentOut]
createTestAgentOuts = [ao1, ao2, ao3]
    where
        ao1 = AgentOut{ agentOutState = AgentState { agentId = 0,
                                                     agentPos = (0.25, 0.5),
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

-- NOTE: used to freeze a given output: render it until the window is closed
testRender :: [AgentOut] -> IO ()
testRender aos = do
    continue <- Front.renderFrame aos
    if continue then
        testRender aos
        else
            return ()