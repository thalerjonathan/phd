module DoubleAuction.Run (
    runDoubleAuctionSteps,
    runDoubleAuctionDebug
  ) where

import DoubleAuction.Model
import DoubleAuction.Init

import FRP.FrABS

import Text.Printf

import System.IO

rngSeed = 42
samplingTimeDelta = 1.0
agentCount = 10
steps = 1000
updateStrat = Parallel -- NOTE: would not work correctly when using Sequential traversion
shuffleAgents = False

runDoubleAuctionSteps :: IO ()
runDoubleAuctionSteps = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initDoubleAuction agentCount
        
        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps

        let (asFinal, envFinal) = last asenv
        mapM printTraderAgent asFinal

        return ()

runDoubleAuctionDebug :: IO ()
runDoubleAuctionDebug = 
    do
        hSetBuffering stdout NoBuffering

        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initDoubleAuction agentCount
        
        processDebug initAdefs initEnv params samplingTimeDelta renderFunc

    where
        renderFunc :: Bool -> ([DAAgentObservable], DAEnvironment) -> IO Bool
        renderFunc _ (aobs, env) = mapM_ printTraderAgent aobs >> (return False)

printTraderAgent :: DAAgentObservable -> IO ()
printTraderAgent (aid, s)
    | isTrader $ s = 
        do
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