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
import Control.Monad.Reader
import Control.Monad.State.Strict
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

diffusePolution :: (MonadState SugEnvironment m)
                => Double 
                -> m ()
diffusePolution t 
    | timeReached && _enablePolution_ = updateCellsM (\c -> c { sugEnvPolutionLevel = 0.0 })
    | otherwise = return ()
  where
    timeReached = mod (floor t) diffusePolutionTime == 0

regrowSugar :: (MonadState SugEnvironment m)
            => Double 
            -> m ()
regrowSugar rate
    | rate < 0 = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: (MonadState SugEnvironment m)
                      => m ()
    regrowSugarByRate  
      = updateCellsM (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  (sugEnvSugarLevel c) + rate})

    regrowSugarToMax :: (MonadState SugEnvironment m) => m ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrowSpice :: (MonadState SugEnvironment m)
            => Double
            -> m ()
regrowSpice rate
    | rate < 0 = regrowSpiceToMax
    | otherwise = regrowSpiceByRate
  where
    regrowSpiceByRate :: (MonadState SugEnvironment m)
                      => m ()
    regrowSpiceByRate 
      = updateCellsM (\c -> 
        c { sugEnvSpiceLevel = 
              min
                  (sugEnvSpiceCapacity c)
                  (sugEnvSpiceLevel c) + rate})

    regrowSpiceToMax :: (MonadState SugEnvironment m)
                     => m ()
    regrowSpiceToMax 
      = updateCellsM (\c -> c { sugEnvSpiceLevel = sugEnvSpiceCapacity c })

regrowSugarByRateAndRegion :: (MonadState SugEnvironment m)
                           => Discrete2dDimension 
                           -> Double 
                           -> m ()
regrowSugarByRateAndRegion range rate 
    = updateCellsWithCoordsM (regrowCell range)                        
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

regrowSpiceByRateAndRegion :: (MonadState SugEnvironment m)
                           => Discrete2dDimension 
                           -> Double 
                           -> m ()
regrowSpiceByRateAndRegion range rate 
    = updateCellsWithCoordsM (regrowCell range)
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

regrowSeasons :: (MonadState SugEnvironment m)
              => Double 
              -> m ()
regrowSeasons t = do
    (_, maxY) <- dimensionsDisc2dM
    
    let halfY = floor (toRational (fromIntegral maxY :: Int) / 2.0 )
    let summerRange = if summerOnTop then (1, halfY) else (halfY + 1, maxY)
    let winterRange = if winterOnTop then (1, halfY) else (halfY + 1, maxY)

    regrowSugarByRateAndRegion summerRange sugarSummerRate
    regrowSugarByRateAndRegion winterRange sugarWinterRate

    when _enableSpice_ (regrowSpiceByRateAndRegion summerRange spiceSummerRate)
    when _enableSpice_ (regrowSpiceByRateAndRegion winterRange spiceWinterRate)
  where
    r = floor (t / seasonDuration) :: Int
    summerOnTop = even r
    winterOnTop = not summerOnTop

    sugarSummerRate = sugarGrowbackUnits / summerSeasonSugarGrowbackRatio
    sugarWinterRate = sugarGrowbackUnits / winterSeasonSugarGrowbackRatio

    spiceSummerRate = spiceGrowbackUnits / summerSeasonSpiceGrowbackRatio
    spiceWinterRate = spiceGrowbackUnits / winterSeasonSpiceGrowbackRatio 

regrowRates :: (MonadState SugEnvironment m)
            => m ()
regrowRates = regrowSugar sugarGrowbackUnits >> when _enableSpice_ (regrowSpice spiceGrowbackUnits)

regrow :: (MonadState SugEnvironment m)
       => Double 
       -> m ()
regrow t = ifThenElse _enableSeasons_ (regrowSeasons t) regrowRates

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  t <- time -< ()
  ctx <- arrM_ (lift ask) -< ()
  let envVar = sugCtxEnv ctx
  env <- arrM (lift . lift . lift . readTVar) -< envVar

  (_, env') <- arrM (\(t, env) -> lift $ runStateT (do
    diffusePolution t
    regrow t) env) -< (t, env)

  arrM (\(envVar, env') -> lift $ lift $ lift $ writeTVar envVar env') -< (envVar, env')

  returnA -< agentOut