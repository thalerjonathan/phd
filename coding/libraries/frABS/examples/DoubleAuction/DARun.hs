module DoubleAuction.DARun where

import DoubleAuction.DAModel
import DoubleAuction.DAInit

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Utils

import Text.Printf

import System.IO
import System.Random

rngSeed = 42
samplingTimeDelta = 1.0
agentCount = 10
steps = 1000
updateStrat = Parallel -- NOTE: should not work correctly when using Sequential traversion
envCollapsing = Nothing   -- NOTE: double-auction is not using a modifyable environment => no need for collapsing 
shuffleAgents = False

runDoubleAuctionSteps :: IO ()
runDoubleAuctionSteps = 
    do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering

        initRng rngSeed

        (initAdefs, initEnv) <- initDoubleAuction agentCount
        params <- initSimParams updateStrat envCollapsing shuffleAgents
        
        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps

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