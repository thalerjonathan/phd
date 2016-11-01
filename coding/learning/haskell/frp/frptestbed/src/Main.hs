{-# LANGUAGE Arrows #-}

module Main where

import Control.Monad
import FRP.Yampa
import Debug.Trace

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
  return (realToFrac dt, Just ())
-}



{------------------------------------------------------------------------------------------------------------
-- FRP cooling-example
cooling :: Double -> SF () (Double)
cooling t0 = proc input -> do
               t0' <- integral >>^ (+ t0) -< -1
               returnA -< t0'

coolingWithFloor :: Double -> SF () (Double)
coolingWithFloor t0 = switch cooling' atRoomTemp
    where cooling' = proc _ -> do
                       t' <- cooling t0 -< ()
                       e <- edge -< t' <= 18
                       returnA -< (t', e `tag` t')
          atRoomTemp _ = (constant 18)

main :: IO ()
main = reactimate (return ())
              (\_ -> return (0.2, Nothing))
              (\_ b -> (putStrLn $ show b) >> return (b <= 18))
              (coolingWithFloor 25.0)
-}

cooling :: SF (Double) (Double)
cooling = proc input -> do
               returnA -< input

main :: IO ()
main = reactimate (return (1.0))
              (\_ -> return (0.2, Nothing))
              (\_ b -> (putStrLn $ show b) >> return (b <= 0.0))
              cooling