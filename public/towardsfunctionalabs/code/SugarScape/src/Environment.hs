{-# LANGUAGE Arrows #-}
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

import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import AgentMonad
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

diffusePolution :: RandomGen g
                => Double 
                -> StateT SugEnvironment (Rand g) ()
diffusePolution t 
    | timeReached && _enablePolution_ = updateCellsM (\c -> c { sugEnvPolutionLevel = 0.0 })
    | otherwise = return ()
  where
    timeReached = mod (floor t) diffusePolutionTime == 0

regrowSugar :: RandomGen g
            => Double 
            -> StateT SugEnvironment (Rand g) ()
regrowSugar rate
    | rate < 0 = regrowSugarToMax
    | otherwise = regrowSugarByRate
  where
    regrowSugarByRate :: RandomGen g
                      => StateT SugEnvironment (Rand g) ()
    regrowSugarByRate  
      = updateCellsM (\c -> 
        c { sugEnvSugarLevel = 
              min
                  (sugEnvSugarCapacity c)
                  (sugEnvSugarLevel c) + rate})

    regrowSugarToMax :: StateT SugEnvironment (Rand g) ()
    regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrowSpice :: RandomGen g
            => Double
            -> StateT SugEnvironment (Rand g) ()
regrowSpice rate
    | rate < 0 = regrowSpiceToMax
    | otherwise = regrowSpiceByRate
  where
    regrowSpiceByRate :: RandomGen g 
                      => StateT SugEnvironment (Rand g) ()
    regrowSpiceByRate 
      = updateCellsM (\c -> 
        c { sugEnvSpiceLevel = 
              min
                  (sugEnvSpiceCapacity c)
                  (sugEnvSpiceLevel c) + rate})

    regrowSpiceToMax :: RandomGen g
                     => StateT SugEnvironment (Rand g) ()
    regrowSpiceToMax 
      = updateCellsM (\c -> c { sugEnvSpiceLevel = sugEnvSpiceCapacity c })

regrowSugarByRateAndRegion :: RandomGen g
                           => Discrete2dDimension 
                           -> Double 
                           -> StateT SugEnvironment (Rand g) ()
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

regrowSpiceByRateAndRegion :: RandomGen g 
                           => Discrete2dDimension 
                           -> Double 
                           -> StateT SugEnvironment (Rand g) ()
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

regrowSeasons :: RandomGen g 
              => Double 
              -> StateT SugEnvironment (Rand g) ()
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

regrowRates :: RandomGen g 
            => StateT SugEnvironment (Rand g) ()
regrowRates = regrowSugar sugarGrowbackUnits >> when _enableSpice_ (regrowSpice spiceGrowbackUnits)

regrow :: RandomGen g 
       => Double 
       -> StateT SugEnvironment (Rand g) ()
regrow t = ifThenElse _enableSeasons_ (regrowSeasons t) regrowRates

sugEnvironment :: RandomGen g 
               => SugAgent g
sugEnvironment = proc _ -> do
  t <- time -< ()
  arrM (lift . lift . diffusePolution) -< t
  arrM (lift . lift . regrow) -< t
  returnA -< agentOut