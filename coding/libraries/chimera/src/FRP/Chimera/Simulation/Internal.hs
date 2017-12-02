module FRP.Chimera.Simulation.Internal 
  ( 
    runAndFreezeSF
  , freeze
  , freezeCol

  , incrementAtomically
  , incrementAtomicallyUnsafe
  ) where

import Control.Concurrent.STM
import System.IO.Unsafe

import FRP.Yampa
import FRP.Yampa.InternalCore

-------------------------------------------------------------------------------
-- Running a Signal-Function
-------------------------------------------------------------------------------

runAndFreezeSF :: SF a b -> a -> DTime -> (SF a b, b)
runAndFreezeSF sf0 a0 dt = (sfFrozen, b0)
    where
        (sf', b0) = (sfTF sf0) a0
        sfFrozen = freeze sf' dt

freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
incrementAtomically :: TVar Int -> STM Int
incrementAtomically var = do
  value <- readTVar var
  writeTVar var (value + 1)
  return value

incrementAtomicallyUnsafe :: TVar Int -> Int
incrementAtomicallyUnsafe = unsafePerformIO  . atomically . incrementAtomically

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