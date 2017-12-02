{-# LANGUAGE Arrows #-}
module SugarScape.Environment (
    cellUnoccupied,
    cellOccupied,
    sugarScapeEnvironmentBehaviour
  ) where

import SugarScape.Model

import FRP.FrABS

import Data.Maybe
import Debug.Trace
import Control.Monad
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugarScapeEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupier cell

cellUnoccupied :: SugarScapeEnvCell -> Bool
cellUnoccupied = not . cellOccupied

diffusePolution :: Double -> State SugarScapeEnvironment ()
diffusePolution time 
    | timeReached && _enablePolution_ = updateCellsM (\c -> c { sugEnvPolutionLevel = 0.0 })
    | otherwise = return ()
    where
        timeReached = mod (floor time) diffusePolutionTime == 0

regrowSugar :: Double -> State SugarScapeEnvironment ()
regrowSugar rate
    | rate < 0 = regrowSugarToMax
    | otherwise = regrowSugarByRate rate
    where
        regrowSugarByRate :: Double -> State SugarScapeEnvironment ()
        regrowSugarByRate rate = updateCellsM
                                    (\c -> c {
                                        sugEnvSugarLevel = (
                                            min
                                                (sugEnvSugarCapacity c)
                                                ((sugEnvSugarLevel c) + rate)
                                                )})

        regrowSugarToMax :: State SugarScapeEnvironment ()
        regrowSugarToMax = updateCellsM (\c -> c { sugEnvSugarLevel = sugEnvSugarCapacity c})

regrowSpice :: Double -> State SugarScapeEnvironment ()
regrowSpice rate
    | rate < 0 = regrowSpiceToMax
    | otherwise = regrowSpiceByRate rate
    where
        regrowSpiceByRate :: Double -> State SugarScapeEnvironment ()
        regrowSpiceByRate rate = updateCellsM
                                    (\c -> c {
                                        sugEnvSpiceLevel = (
                                            min
                                                (sugEnvSpiceCapacity c)
                                                ((sugEnvSpiceLevel c) + rate))
                                                })

        regrowSpiceToMax ::  State SugarScapeEnvironment ()
        regrowSpiceToMax = updateCellsM (\c -> c { sugEnvSpiceLevel = sugEnvSpiceCapacity c })

regrowSugarByRateAndRegion :: Discrete2dDimension -> Double -> State SugarScapeEnvironment ()
regrowSugarByRateAndRegion range rate = updateCellsWithCoordsM (regrowCell range)                        
    where
        regrowCell :: Discrete2dDimension -> (Discrete2dCoord, SugarScapeEnvCell) -> SugarScapeEnvCell
        regrowCell (fromY, toY) ((_, y), c)
            | y >= fromY && y <= toY = c {
                                           sugEnvSugarLevel = (
                                               min
                                                   (sugEnvSugarCapacity c)
                                                   ((sugEnvSugarLevel c) + rate))
                                                   }
            | otherwise = c

regrowSpiceByRateAndRegion :: Discrete2dDimension -> Double -> State SugarScapeEnvironment ()
regrowSpiceByRateAndRegion range rate = updateCellsWithCoordsM (regrowCell range)
    where
        regrowCell :: Discrete2dDimension -> (Discrete2dCoord, SugarScapeEnvCell) -> SugarScapeEnvCell
        regrowCell (fromY, toY) ((_, y), c)
            | y >= fromY && y <= toY = c {
                                           sugEnvSpiceLevel = (
                                               min
                                                   (sugEnvSpiceCapacity c)
                                                   ((sugEnvSpiceLevel c) + rate))
                                                   }
            | otherwise = c

regrowSeasons :: Double -> State SugarScapeEnvironment ()
regrowSeasons time = 
    do
        (_, maxY) <- dimensionsDisc2dM
        
        let halfY = floor ((toRational $ fromIntegral maxY) / 2.0 )
        let summerRange = if summerOnTop then (1, halfY) else (halfY + 1, maxY)
        let winterRange = if winterOnTop then (1, halfY) else (halfY + 1, maxY)

        regrowSugarByRateAndRegion summerRange sugarSummerRate
        regrowSugarByRateAndRegion winterRange sugarWinterRate

        when _enableSpice_ (regrowSpiceByRateAndRegion summerRange spiceSummerRate)
        when _enableSpice_ (regrowSpiceByRateAndRegion winterRange spiceWinterRate)

    where
        r = floor (time / seasonDuration)
        summerOnTop = even r
        winterOnTop = not summerOnTop

        sugarSummerRate = sugarGrowbackUnits / summerSeasonSugarGrowbackRatio
        sugarWinterRate = sugarGrowbackUnits / winterSeasonSugarGrowbackRatio

        spiceSummerRate = spiceGrowbackUnits / summerSeasonSpiceGrowbackRatio
        spiceWinterRate = spiceGrowbackUnits / winterSeasonSpiceGrowbackRatio 


regrowRates :: State SugarScapeEnvironment ()
regrowRates = regrowSugar sugarGrowbackUnits >> when _enableSpice_ (regrowSpice spiceGrowbackUnits)

regrow :: Double -> State SugarScapeEnvironment ()
regrow time = ifThenElse _enableSeasons_ (regrowSeasons time) regrowRates

behaviourM :: SugarScapeEnvironmentMonadicBehaviour
behaviourM time = 
    do
        diffusePolution time
        regrow time
        return $ trace ("Time = " ++ show time) ()

sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour = environmentMonadic behaviourM