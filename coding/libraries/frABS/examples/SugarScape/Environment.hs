{-# LANGUAGE Arrows #-}
module SugarScape.Environment (
    cellUnoccupied,
    cellOccupied,
    sugarScapeEnvironmentBehaviour
  ) where

import SugarScape.Model

import FRP.FrABS

import FRP.Yampa

import Data.Maybe
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugarScapeEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupier cell

cellUnoccupied :: SugarScapeEnvCell -> Bool
cellUnoccupied = not . cellOccupied

diffusePolution :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
diffusePolution t env
    | timeReached = updateCells (\c -> c { sugEnvPolutionLevel = 0.0 }) env
    | otherwise = env
    where
        timeReached = mod (floor t) diffusePolutionTime == 0

regrowSugarByRate :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarByRate rate env = updateCells
                                (\c -> c {
                                    sugEnvSugarLevel = (
                                        min
                                            (sugEnvSugarCapacity c)
                                            ((sugEnvSugarLevel c) + rate))
                                            })
                                env

regrowSpiceByRate :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceByRate rate env = updateCells
                                (\c -> c {
                                    sugEnvSpiceLevel = (
                                        min
                                            (sugEnvSpiceCapacity c)
                                            ((sugEnvSpiceLevel c) + rate))
                                            })
                                env

regrowSugarToMax ::  SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarToMax env = updateCells
                            (\c -> c {
                                sugEnvSugarLevel = (sugEnvSugarCapacity c)})
                            env

regrowSpiceToMax ::  SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceToMax env = updateCells
                            (\c -> c {
                                sugEnvSpiceLevel = (sugEnvSpiceCapacity c)})
                            env

regrowSugarByRateAndRegion :: Discrete2dDimension -> Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarByRateAndRegion range rate env = updateCellsWithCoords
                                            (regrowCell range)
                                            env                          
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

regrowSpiceByRateAndRegion :: Discrete2dDimension -> Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceByRateAndRegion range rate env = updateCellsWithCoords
                                            (regrowCell range)
                                            env
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

environmentSeasons :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
environmentSeasons t env = envWinterRegrow
    where
        r = floor (t / seasonDuration)
        summerTop = even r
        winterTop = not summerTop
        summerRange = if summerTop then (1, halfY) else (halfY + 1, maxY)
        winterRange = if winterTop then (1, halfY) else (halfY + 1, maxY)

        envSummerRegrow = regrowSpiceByRateAndRegion summerRange (spiceGrowbackUnits / summerSeasonSpiceGrowbackRate) 
                            (regrowSugarByRateAndRegion summerRange (sugarGrowbackUnits / summerSeasonSugarGrowbackRate) env)
        envWinterRegrow = regrowSpiceByRateAndRegion winterRange (spiceGrowbackUnits / winterSeasonSpiceGrowbackRate)
                            (regrowSugarByRateAndRegion winterRange (sugarGrowbackUnits / winterSeasonSugarGrowbackRate) envSummerRegrow)

        (_, maxY) = envDisc2dDims env
        halfY = floor ((toRational $ fromIntegral maxY) / 2.0 )

sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour = proc env ->
    do
        t <- time -< 0

        let env' = if polutionEnabled then diffusePolution t env else env

        let envSeasonal = environmentSeasons t env'
        let envRegrowByRate = regrowSpiceByRate spiceGrowbackUnits (regrowSugarByRate sugarGrowbackUnits env')
        let envRegrowToMax = regrowSugarToMax (regrowSugarToMax env')

        returnA -< trace ("Time = " ++ (show t)) envRegrowByRate