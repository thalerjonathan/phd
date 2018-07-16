{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module Environment 
  (
    cellUnoccupied
  , cellOccupied
  
  , regrowRates
  , regrow

  , diffusePolution

  , sugEnvironment
  ) where

import Data.Maybe

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import FRP.BearRiver

import Common
import Discrete
import Model
import Utils

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupier cell

cellUnoccupied :: SugEnvCell -> Bool
cellUnoccupied = not . cellOccupied

diffusePolution :: Double 
                -> SugEnvironment
                -> STM ()
diffusePolution t env
    | timeReached && _enablePolution_ = updateAllCells (\c -> c { sugEnvPolutionLevel = 0.0 }) env
    | otherwise = return ()
  where
    timeReached = mod (floor t) diffusePolutionTime == 0

regrowSugar :: Double 
            -> SugEnvironment
            -> STM ()
regrowSugar rate env
    | rate < 0  = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: STM ()
    regrowSugarByRate  
      = updateAllCells (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  (sugEnvSugarLevel c) + rate}) env

    regrowSugarToMax :: STM ()
    regrowSugarToMax = updateAllCells (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c}) env

regrowSpice :: Double 
            -> SugEnvironment
            -> STM ()
regrowSpice rate env
    | rate < 0  = regrowSpiceToMax
    | otherwise = regrowSpiceByRate
  where
    regrowSpiceByRate :: STM ()
    regrowSpiceByRate 
      = updateAllCells (\c -> 
        c { sugEnvSpiceLevel = 
              min
                  (sugEnvSpiceCapacity c)
                  (sugEnvSpiceLevel c) + rate}) env

    regrowSpiceToMax :: STM ()
    regrowSpiceToMax 
      = updateAllCells (\c -> c { sugEnvSpiceLevel = sugEnvSpiceCapacity c }) env

regrowSugarByRateAndRegion :: Discrete2dDimension 
                           -> Double 
                           -> SugEnvironment
                           -> STM ()
regrowSugarByRateAndRegion range rate env
    = updateAllCellsWithCoords (regrowCell range) env
  where
    regrowCell :: Discrete2dDimension 
               -> (Discrete2dCoord, SugEnvCell) 
               -> SugEnvCell
    regrowCell (fromY, toY) ((_, y), c)
      | y >= fromY && y <= toY = c {
                                      sugEnvSugarLevel = 
                                          min
                                              (sugEnvSugarCapacity c)
                                              (sugEnvSugarLevel c) + rate}
      | otherwise = c

regrowSpiceByRateAndRegion :: Discrete2dDimension 
                           -> Double 
                           -> SugEnvironment
                           -> STM ()
regrowSpiceByRateAndRegion range rate env
    = updateAllCellsWithCoords (regrowCell range) env
  where
    regrowCell :: Discrete2dDimension
               -> (Discrete2dCoord, SugEnvCell) 
               -> SugEnvCell
    regrowCell (fromY, toY) ((_, y), c)
      | y >= fromY && y <= toY = c {
                                      sugEnvSpiceLevel = 
                                          min
                                              (sugEnvSpiceCapacity c)
                                              (sugEnvSpiceLevel c) + rate}
      | otherwise = c

regrowSeasons :: Double 
              -> SugEnvironment
              -> STM ()
regrowSeasons t env = do
    let (_, maxY)   = dimensionsDisc2d env
        halfY       = floor (toRational (fromIntegral maxY :: Int) / 2.0 )
        summerRange = if summerOnTop then (1, halfY) else (halfY + 1, maxY)
        winterRange = if winterOnTop then (1, halfY) else (halfY + 1, maxY)

    regrowSugarByRateAndRegion summerRange sugarSummerRate env
    regrowSugarByRateAndRegion winterRange sugarWinterRate env

    when _enableSpice_ (regrowSpiceByRateAndRegion summerRange spiceSummerRate env)
    when _enableSpice_ (regrowSpiceByRateAndRegion winterRange spiceWinterRate env)
  where
    r = floor (t / seasonDuration) :: Int
    summerOnTop = even r
    winterOnTop = not summerOnTop

    sugarSummerRate = sugarGrowbackUnits / summerSeasonSugarGrowbackRatio
    sugarWinterRate = sugarGrowbackUnits / winterSeasonSugarGrowbackRatio

    spiceSummerRate = spiceGrowbackUnits / summerSeasonSpiceGrowbackRatio
    spiceWinterRate = spiceGrowbackUnits / winterSeasonSpiceGrowbackRatio 

regrowRates :: SugEnvironment
            -> STM ()
regrowRates env = do
  regrowSugar sugarGrowbackUnits env
  when _enableSpice_ 
    (regrowSpice spiceGrowbackUnits env)

regrow :: Double 
       -> SugEnvironment
       -> STM ()
regrow t env 
  = ifThenElse _enableSeasons_ 
    (regrowSeasons t env) 
    (regrowRates env)

runEnv :: Double 
       -> SugEnvironment
       -> STM ()
runEnv t env = do
  diffusePolution t env
  regrow t env

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  t <- time -< ()
  env <- arrM_ (lift getEnvironment) -< ()

  arrM (\(t, env) -> lift $ lift $ lift $ runEnv t env) -< (t, env)

  returnA -< agentOut