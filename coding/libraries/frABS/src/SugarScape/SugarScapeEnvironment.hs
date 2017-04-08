{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeEnvironment where

-- Project-internal import first
import SugarScape.SugarScapeModel

import FrABS.Env.Environment
import FrABS.Agent.Agent

-- Project-specific libraries follow
import FRP.Yampa

import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
cellOccupied :: SugarScapeEnvCell -> Bool
cellOccupied cell = isJust $ sugEnvOccupied cell

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

regrowSugarToMax ::  SugarScapeEnvironment -> SugarScapeEnvironment
regrowSugarToMax env = updateEnvironmentCells
                            env
                            (\c -> c {
                                sugEnvSugarLevel = (sugEnvSugarCapacity c)} )

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

environmentSeasons :: Double -> SugarScapeEnvironment -> SugarScapeEnvironment
environmentSeasons t env = envWinterRegrow
    where
        r = floor (t / seasonDuration)
        summerTop = even r
        winterTop = not summerTop
        summerRange = if summerTop then (1, halfY) else (halfY + 1, maxY)
        winterRange = if winterTop then (1, halfY) else (halfY + 1, maxY)
        envSummerRegrow = regrowSugarByRateAndRegion summerRange (sugarGrowbackUnits / summerSeasonGrowbackRate) env
        envWinterRegrow = regrowSugarByRateAndRegion winterRange (sugarGrowbackUnits / winterSeasonGrowbackRate) envSummerRegrow
        (_, maxY) = envLimits env
        halfY = floor ((toRational $ fromIntegral maxY) / 2.0 )

sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour = proc env ->
    do
        t <- time -< 0

        --let envPolutionDiffusion = diffusePolution t env
        let envPolutionDiffusion = env

        let envSeasonal = environmentSeasons t envPolutionDiffusion
        let envRegrowSugarByRate = regrowSugarByRate sugarGrowbackUnits envPolutionDiffusion
        let envRegrowSugarToMax = regrowSugarToMax envPolutionDiffusion

        returnA -< trace ("Time = " ++ (show t)) envRegrowSugarByRate