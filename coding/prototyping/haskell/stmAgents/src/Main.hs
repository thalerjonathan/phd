{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent
import Control.Monad

import Control.Monad.STM
import Control.Concurrent.STM.Stats
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar

-- import Debug.Trace

type Time = Int
data Msg  = Tick Time | Ping String (TQueue Msg) | Pong String 

main :: IO ()
main = do
  q1 <- ("Jonathan",) <$> newTQueueIO 
  q2 <- ("Martin",) <$> newTQueueIO
  q3 <- ("Jamie",) <$> newTQueueIO

  let qs = [q1, q2, q3]
  (rcvs, snds) <- unzip <$> mapM (\q -> startAgent q qs) qs
  
  let tMax = 1000

  forM_ [1..tMax] $ \t -> do
    -- writing ticks to all agents-queues: because of MVars
    -- it is guaranteed that the agents are waiting at this 
    -- point
    putStrLn $ "Starting Tick t = " ++ show t ++ " ..."
    -- unblock agents, so they can react to tick 
    mapM_ (`putMVar` t) rcvs
    -- wait for agents to finish...
    ppcs <- mapM takeMVar snds
    -- agents have finished, print info and ping-pong counters
    putStrLn $ "Tick t = " ++ show t ++ " has finished"
    mapM_ (\(ppc, (name, _)) -> putStrLn ("\t" ++ name ++ " ppc: \t" ++ show ppc)) (zip ppcs qs)
    -- at this point all agents have finished the current tick
    -- print a break-line
    putStrLn ""
  
  dumpSTMStats

writeTick :: Int -> TQueue Msg -> IO ()
writeTick t q = trackSTM $ writeTQueue q (Tick t)

startAgent :: (String, TQueue Msg)
           -> [(String, TQueue Msg)]      
           -> IO (MVar Time, MVar Int)
startAgent self@(_myName, q) qs = do
  tickRcv <- newEmptyMVar
  tickSnd <- newEmptyMVar
  -- initialise the ping-pong counter 
  -- which counts the number of finished ping-pongs
  ppcVar <- newTVarIO 0

  let allQueues = map snd (self : qs)
  
  _ <- forkIO $ forever $ do
    -- wait for main-thread to send next tick time
    t <- takeMVar tickRcv 
    -- next tick has arrived, write it to my queue
    writeTick t q
    -- reset ping-pong counter to 0
    trackSTM $ writeTVar ppcVar 0
    -- check for messages until ALL queues are empty
    whileM 
      (notM $ allQueuesEmpty allQueues)
      (do --putStrLn $ myName ++ " received tick, start working..."
      _ <- trackSTM $ agentAction self qs ppcVar
      return ())
    -- all messages are processed, get ping-pong counter
    ppc <- readTVarIO ppcVar
    -- signal main-thread that this thread has finished 
    -- by posting ping-pong counter
    putMVar tickSnd ppc

  return (tickRcv, tickSnd)

allQueuesEmpty :: [TQueue Msg] -> IO Bool
allQueuesEmpty qs = do
  atomically $ foldM (\flag q -> ((&&) flag) <$> isEmptyTQueue q) True qs

queueEmpty :: TQueue Msg -> IO Bool
queueEmpty q = atomically $ isEmptyTQueue q

agentAction :: (String, TQueue Msg)
            -> [(String, TQueue Msg)]
            -> TVar Int
            -> STM ()
agentAction (myName, qSelf) qs ppcVar = do
    mmsg <- tryReadTQueue qSelf 
    case mmsg of
      Nothing -> return ()
      Just msg -> case msg of
        Tick _t -> do
          --trace (myName ++ ": received Tick with t = " ++ show t)
            mapM_ startPingPongWith qs

        Ping _senderName senderQueue -> do
          --trace (myName ++ " received Ping from " ++ senderName ++ ", reply with Pong...")
            writeTQueue senderQueue (Pong myName)

        Pong _senderName -> do
          --trace (myName ++ " received Pong message from " ++ senderName ++ ", Ping-Pong finished!") 
            modifyTVar ppcVar (+1)
  where
    startPingPongWith :: (String, TQueue Msg) -> STM ()
    startPingPongWith (name, q) 
      | name == myName = return ()
      | otherwise = do
        --trace (myName ++ ": sending Ping to " ++ name ++ "...")  
          writeTQueue q (Ping myName qSelf)

notM :: Monad m
     => m Bool
     -> m Bool
notM f = liftM (not) f

whileM :: Monad m
       => m Bool
       -> m a
       -> m ()
whileM f act = do
  ret <- f
  if ret 
    then do
      _ <- act
      whileM f act
    else return ()