module VisualRunner 
  (
    runSDVisual
  ) where

import FRP.Yampa

import SIR

runSDVisual :: Double
            -> Double
            -> Double
            -> Double
            -> Double
            -> Time
            -> DTime
            -> IO ()
runSDVisual populationSize infectedCount contactRate infectivity illnessDuration t dt = do
    hdl <- reactInit 
            (return ()) 
            stepSIR
            (sir populationSize infectedCount contactRate infectivity illnessDuration)

    -- TODO: plot using http://hackage.haskell.org/package/Chart
    -- TODO: render stocks and flows using gloss
    nextStep steps hdl dt
  where
    steps = floor $ t / dt

nextStep :: Int 
         -> ReactHandle () SIRStep
         -> DTime
         -> IO ()
nextStep 0 _ _ = return ()
nextStep n hdl dt = do
  _ <- react hdl (dt, Nothing)
  nextStep (n - 1) hdl dt

stepSIR :: ReactHandle () SIRStep
        -> Bool 
        -> SIRStep
        -> IO Bool
stepSIR _hdl _ out = do
-- TODO: stop after given number of steps
  print out
  return True