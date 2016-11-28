module Main where

import FRP.Yampa
import System.Environment (getArgs, getProgName)

import HACAgent as Agent
import HACSimulation as Sim
import HACFrontend as Front

import HACYampaBackend as YampaBack
import HACClassicBackend as ClassicBack

heroDistribution :: Double
heroDistribution = 1.0

dimensions :: (Int, Int)
dimensions = (800, 800)

main :: IO ()
main = do
    Front.initialize
    as <- Agent.createAgents 100 heroDistribution
    -- ClassicBack.process as (\as -> Front.renderFrame (map agentPos as))
    let as' = ClassicBack.process_ as 100000
    Front.renderFrame (map agentPos as')
    Front.shutdown


-----------------------------------------------------------------------------------------------------------------------
-- YAMPA --
-----------------------------------------------------------------------------------------------------------------------
yampaRun :: IO ()
yampaRun = do
    Front.initialize
    agents <- Agent.createAgents 100 heroDistribution
    reactimate Main.initialize input output (YampaBack.process agents)
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