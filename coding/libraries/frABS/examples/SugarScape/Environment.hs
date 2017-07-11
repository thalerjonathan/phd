{-# LANGUAGE Arrows #-}
module SugarScape.Environment (
    cellUnoccupied,
    cellOccupied,
    sugarScapeEnvironmentBehaviour
  ) where

import SugarScape.Model
import FrABS.Env.Environment

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
    | timeReached = updateEnvironmentCells
                                           env
                                           (\c -> c {
                                               sugEnvPolutionLevel = 0.0} )
    | otherwise = env
    where
        timeReached = mod (floor t) diffusePolutionTime == 0

regrowSugarByRate :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarByRate rate env = updateEnvironmentCells
                                env
                                (\c -> c {
                                    sugEnvSugarLevel = (
                                        min
                                            (sugEnvSugarCapacity c)
                                            ((sugEnvSugarLevel c) + rate))
                                            } )

regrowSpiceByRate :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceByRate rate env = updateEnvironmentCells
                                env
                                (\c -> c {
                                    sugEnvSpiceLevel = (
                                        min
                                            (sugEnvSpiceCapacity c)
                                            ((sugEnvSpiceLevel c) + rate))
                                            } )

regrowSugarToMax ::  SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarToMax env = updateEnvironmentCells
                            env
                            (\c -> c {
                                sugEnvSugarLevel = (sugEnvSugarCapacity c)} )

regrowSpiceToMax ::  SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceToMax env = updateEnvironmentCells
                            env
                            (\c -> c {
                                sugEnvSpiceLevel = (sugEnvSpiceCapacity c)} )

regrowSugarByRateAndRegion :: (Int, Int) -> Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarByRateAndRegion range rate env = updateEnvironmentCellsWithCoords
                                            env
                                            (regrowCell range)
    where
        regrowCell :: (Int, Int) -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeEnvCell
        regrowCell (fromY, toY) ((_, y), c)
            | y >= fromY && y <= toY = c {
                                           sugEnvSugarLevel = (
                                               min
                                                   (sugEnvSugarCapacity c)
                                                   ((sugEnvSugarLevel c) + rate))
                                                   }
            | otherwise = c

regrowSpiceByRateAndRegion :: (Int, Int) -> Double -> SugarScapeEnvironment -> SugarScapeEnvironment
regrowSpiceByRateAndRegion range rate env = updateEnvironmentCellsWithCoords
                                            env
                                            (regrowCell range)
    where
        regrowCell :: (Int, Int) -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeEnvCell
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

        (_, maxY) = envLimits env
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