{-# LANGUAGE Arrows #-}

module SIRFRP where

import Control.Monad
import System.Random
import FRP.Yampa

{- SIGNATURE of reactimate
reactimate :: IO a                          -- init
           -> (Bool -> IO (DTime, Maybe a)) -- input/sense
           -> (Bool -> b -> IO Bool)        -- output/actuate
           -> SF a b                        -- process/signal function
           -> IO ()
-}

{------------------------------------------------------------------------------------------------------------
-- FRP hello-world example 
import Data.IORef
import Data.Time.Clock

twoSecondsPassed :: SF () Bool
twoSecondsPassed = time >>> arr (> 2)

main :: IO ()
main = do
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) actuate twoSecondsPassed
 
initialize :: IO ()
initialize = putStrLn "Hello... wait for it..." 
 
actuate :: Bool -> Bool -> IO Bool
actuate _ x = when x (putStrLn "World!") >> return x
--actuate _ x
--  | x = (putStrLn "World!") >> return x
--  | otherwise = (putStrLn "waiting...") >> return x

sense :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
sense timeRef _ = do
  (putStrLn "sensing...")
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac df, Just ())
-}

data SIRState = Susceptible | Infected | Recovered

data SIRAgent = SIRAgent {
  agentState :: SIRState,
  daysInfected :: Int
  }

main :: IO ()
main = do
  reactimate initialize sense actuate agentRecovered

initialize :: IO SIRAgent
initialize = return SIRAgent { agentState = Infected, daysInfected = 3 }

sense :: Bool -> IO (Double, Maybe SIRAgent)
sense  f = return (1.0, Nothing)

actuate :: Bool -> Bool -> IO Bool
actuate _ a = return a

agentRecovered :: SF SIRAgent Bool
agentRecovered = proc a -> do
  returnA -< (daysInfected a >= 0)
