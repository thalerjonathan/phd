module Main where

import           AgentZero.Run
import           Conversation.Run
import           DoubleAuction.Run
import           FrSIRSNetwork.Run
import           FrSIRSSpatial.Run
import           HeroesCowards.Run
import           PolicyEffects.Run
import           PrisonersDilemma.Run
import           RecursiveABS.Run
import           Segregation.Run
import           SIRS.Run
import           SugarScape.Run
import           SysDynSIR.Run
import           Wildfire.Run

{-
    TODOs   
    - implement continuous / discrete environment
        -> network is still available in both continuous and discrete environment and independent of it
        -> do we have to make a distinction between a continuous / discrete agent as it has different operations available ?
        -> don't want to rewrite all general-purpose functions which are independent of the environment (e.g. send message) 
        -> dirty hack solution: introduce aoPosCont :: (Double, Double) and aoPosDisc :: (Int, Int)

    - clean-up
        - imports: no unused imports
        - lint: must be clear of warnings
        - warnings: compilation with -w must show no warnings at all
        
    - comment haskell code
-}

class Test a where
    blaBeh :: a -> Bool

instance Test String where
    blaBeh "Jonathan" = True
    blaBeh _ = False

main :: IO ()
main = runHeroesCowardsWithRendering

    -- runPolicyEffectsWithRendering
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
