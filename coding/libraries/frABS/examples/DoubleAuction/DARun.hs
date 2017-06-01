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

agentCount = 10
steps = 1000

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

        mapM printTraderAgent asFinal
        return ()

printTraderAgent :: DAAgentOut -> IO ()
printTraderAgent a 
    | isTrader $ aoState a = 
        do
            let aid = aoId a

            let s = aoState a
            let cash = daTraderCash s
            let assets = daTraderAssets s
            let loansTaken = daTraderLoansTaken s
            let loansGiven = daTraderLoansGiven s

            putStrLn $ "Agent " ++ (show aid) 
                                ++ ": cash = " ++ (show cash) 
                                ++ " assets = " ++ (show assets)
                                ++ " loans taken = " ++ (show loansTaken)
                                ++ " loans given = " ++ (show loansGiven)

            return ()
    | otherwise = return ()

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g