module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun
import SIRS.SIRSRun
import FrSIRSSpatial.FrSIRSSpatialRun
import FrSIRSNetwork.FrSIRSNetworkRun
import SysDynSIR.SysDynSIRRun
import Segregation.SegregationRun
import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import DoubleAuction.DARun
import Wildfire.WildfireRun
import PrisonersDilemma.PDRun

{-
	TODOs
    - clean-up all imports
        -  ALL modules explicitly export their stuff (also when they export everything)
        
    - replications not working in case of FrSIRS because RNGs are fixed at agent-creation time and hardwired into the Behaviour => always use the same RNGs
    -  fix problem of replications: rngs are always the same. pass 2 functions which run in the Rand Monad and gets passed the original agentdef to create a new agentdef and one to create a new envoronment
    
    - use-case for continuous 2d-environment: implement Heroes & Cowards
        -> write Agend2DContinuous
        - continuous 2d env: just add a map of agentids with their positions to the env, agents can then update their continuous position (needs to remove itself when killed). problem: environment needs to know about agentid. but do we really need that? it would save us exchanging messages.
        - basically it would suffice to add another field: posCont and make the other posDisc. or can we distinguish by types the position: any num type
        - maybe distinguish between discrete agent and continuous agent
        - distinguish between cont and disc env

    - performance?
    - compilation with -w must show no warnings at all
    - clean-up imports
    - clean-up structure: lint
    - comment haskell-code
    - reuse the Agent2D renderer if appropriate

	- Model-specification using QuickCheck
-}

main :: IO ()
main = runFrSIRSNetworkWithRendering

    -- runFrSIRSNetworkStepsAndWriteToFile -- runFrSIRSNetworkWithRendering -- runFrSIRSNetworkReplicationsAndWriteToFile
    -- runSysDynSIRStepsAndWriteToFile
    -- runFrSIRSSpatialWithRendering -- runFrSIRSSpatialStepsAndPrint -- runFrSIRSSpatialStepsAndWriteToFile
    -- runDoubleAuctionSteps
    -- runSIRSWithRendering
    -- runPDWithRendering
    -- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
    -- runMetaABSStepsAndPrint
    -- runSegWithRendering

{-
import System.IO
import System.IO.Unsafe
import System.Random

import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.Stats

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    var <- newTVarIO 0
    mapM (\i -> forkIO $
        do 
            threadId <- myThreadId
            forever $
                do
                    randDelay <- getStdRandom (randomR (100000,500000))
                    threadDelay randDelay

                    -- value <- atomically $ incrementAtomically var
                    --value <- trackSTM $ incrementAtomically var
                    let value = incrementAtomicallyUnsafe var

                    putStrLn ("Thread " ++ (show threadId) ++ " produced value: " ++ (show value))
            ) [1..10]

    -- wait 10 seconds
    threadDelay 10000000

    value <- readTVarIO var
    putStrLn ("Main reads value: " ++ (show value))

    --dumpSTMStats

incrementAtomically :: TVar Int -> STM Int
incrementAtomically var = 
    do
        value <- readTVar var
        writeTVar var (value + 1)
        return value

incrementAtomicallyUnsafe :: TVar Int -> Int
incrementAtomicallyUnsafe = unsafePerformIO  . atomically . incrementAtomically
-}