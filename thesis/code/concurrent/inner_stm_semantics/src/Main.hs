module Main where

import Control.Concurrent

import Control.Monad.STM
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.State.Strict

import Debug.Trace

main :: IO ()
main = do
  v <- newTVarIO 0
  m <- newEmptyMVar 
  forkIO (testThread v m)

  forM_ [1..42] (\i -> do
    -- use delay to 'make sure' that a retry is happening for ever increment
    threadDelay 10000
    atomically $ writeTVar v i)

  (a, s) <- takeMVar m

  putStrLn $ "final StateT state     = " ++ show s
  putStrLn $ "stm computation result = " ++ show a

testThread :: TVar Int -> MVar (Int, Int) -> IO ()
testThread v m = do
    let s = 0
    let stmAction = runStateT testAction 0
    (a, s') <- atomically stmAction
    putMVar m (a, s')

  where
    testAction :: StateT Int STM Int 
    testAction = do
      trace "retrying, increment!" $ modify (+1)
      n <- lift $ readTVar v
      if n < 42
        then lift retry
        else return n