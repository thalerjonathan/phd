module DoubleAuction.DARun where

import DoubleAuction.DAModel
import DoubleAuction.DAInit

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import System.IO
import System.Random

rngSeed = 42
timeStep = 1.0

agentCount = 3
steps = 1

-- NOTE: double-auction works both for parallel and sequential
parallelStrategy = Just daEnvironmentsCollapse

runDoubleAuctionWithRendering :: IO ()
runDoubleAuctionWithRendering = 
    do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering

        initRng rngSeed

        (as, env) <- initDoubleAuction agentCount

        let asenv = processSteps as env parallelStrategy 1.0 steps
        let (asFinal, envFinal) = last asenv

        mapM printAgent asFinal
        return ()

printAgent :: DAAgentOut -> IO ()
printAgent aout = 
    do
        let s = aoState aout
        let aid = aoId aout

        putStrLn $ "Agent " ++ (show aid) ++ ": " ++ (show s)

        return ()

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g