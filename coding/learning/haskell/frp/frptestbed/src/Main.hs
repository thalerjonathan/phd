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

1. call to initialize
2. return of initialize is fed to process
3. return of process is fed to output
4. input is called which produces the next sample / time or returns Nothing when no change occured
5. the return-value of input is fed to process
6. jump to 3


-- initializes the system: returns data which is then sent to process
initialize :: IO String
initialize  = return "Hello Yampa"

-- receives a Bool which is unused
-- return Tuple with DTime: (time elapsed since last input, Maybe input-data)
input :: Bool -> IO (DTime, Maybe String)
input _ = return (0.0, Nothing)

-- 1st argument: a Bool which is unused
-- 2nd argument: the output from process
-- return True when terminating programm
output :: Bool -> String -> IO Bool
output _  x = putStrLn x >> return True

-- the process-function: maps input-data to output-data
process :: SF String String
process = identity
-}

main :: IO ()
main = do
    hdl <- reactInit
        initialize
        output
        process
    inputLoop hdl
    return ()

initialize :: IO Double
initialize = return 1.0

output :: (ReactHandle Double Double) -> Bool -> Double -> IO Bool
output hdl changed sfOut = do
                    putStrLn (show changed)
                    putStrLn (show sfOut)
                    return True

inputLoop :: ReactHandle Double Double -> IO ()
inputLoop hdl = do
    ret <- react hdl (1.0, Just 1.0)
    if ret == True
        then
            return ()
        else
            inputLoop hdl

process :: SF Double Double
process = proc input -> do
                t0' <- time -< input
                returnA -< t0'
                
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
