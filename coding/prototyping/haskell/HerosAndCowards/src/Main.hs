module Main where

import FRP.Yampa
import System.Environment (getArgs, getProgName)

import HACBackend as Back
import HACFrontend as Front

dimensions :: (Int, Int)
dimensions = (800, 800)

main :: IO ()
main = do
    Front.initialize
    agents <- Back.createAgents 100
    reactimate Main.initialize input output (Back.process agents)
    Front.shutdown

initialize :: IO SimulationIn
initialize = do
    return SimulationIn {}

input :: Bool -> IO (DTime, Maybe SimulationIn)
input _ = return (1.0, Nothing)

output :: Bool -> SimulationOut -> IO Bool
output _ out = do
    let as = simOutAllAgents out
    Front.renderFrame as
    return False