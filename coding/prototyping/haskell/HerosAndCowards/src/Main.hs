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
heroDistribution = 0.0

dimensions :: (Int, Int)
dimensions = (800, 800)

-----------------------------------------------------------------------------------------------------------------------
-- CLASSIC --
-----------------------------------------------------------------------------------------------------------------------
{-
main :: IO ()
main = do
    -- g <- getStdGen
    let g = mkStdGen 42 -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createAgents g 3 heroDistribution
    ClassicBack.process as (\as -> Front.renderFrame (map agentPos as))
    --let as' = ClassicBack.process_ as 100000
    --Front.renderFrame (map agentPos as')
    Front.shutdown
-}

-----------------------------------------------------------------------------------------------------------------------
-- YAMPA --
-----------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    -- g <- getStdGen
    let g = mkStdGen 42 -- NOTE: if we want to reproduce then we need to onitialize RNG ourselves
    Front.initialize
    let as = Agent.createAgents g 100 heroDistribution
    reactimate Main.initialize input output (YampaBack.process as)
    Front.shutdown

initialize :: IO Sim.SimIn
initialize = do
    return Sim.SimIn {}

input :: Bool -> IO (DTime, Maybe Sim.SimIn)
input _ = return (1.0, Nothing)

output :: Bool -> Sim.SimOut -> IO Bool
output _ out = do
    let as = Sim.simOutAllAgents out
    winOpened <- Front.renderFrame as
    return $ not winOpened

-----------------------------------------------------------------------------------------------------------------------