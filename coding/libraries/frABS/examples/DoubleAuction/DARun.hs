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
import Control.Monad.Random
import Text.Printf

rngSeed = 42
timeStep = 1.0

agentCount = 50
steps = 10000

runDoubleAuctionWithRendering :: IO ()
runDoubleAuctionWithRendering = 
    do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering

        initRng rngSeed

        (as, env) <- initDoubleAuction agentCount
        params <- simParams
        
        let asenv = processSteps as env params 1.0 steps
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
                                ++ ": cash = " ++ (printf "%.2f" cash)
                                ++ " assets = " ++ (printf "%.2f" assets)
                                ++ " loans taken = " ++ (printf "%.2f" loansTaken)
                                ++ " loans given = " ++ (printf "%.2f" loansGiven)

            return ()
    | otherwise = return ()

simParams :: IO (SimulationParams DAEnvCell ())
simParams = 
    do
        rng <- getSplit
        return SimulationParams {
            simStrategy = Parallel,     -- NOTE: double-auction works both for parallel and sequential
            simEnvCollapse = Nothing,   -- NOTE: double-auction is not using a modifyable environment => no need for collapsing 
            simShuffleAgents = False,
            simRng = rng
        }

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g