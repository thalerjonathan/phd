module SugarScape.Init (
    createSugarScape
  ) where

import SugarScape.Model
import SugarScape.Agent
import SugarScape.Environment

import FRP.FrABS

import FRP.Yampa

import Data.List

import System.Random
import Control.Monad.Random


createSugarScape :: Int 
                    -> Discrete2dDimension 
                    -> SugarScapeSimParams
                    -> IO ([SugarScapeAgentDef], SugarScapeEnvironment)
createSugarScape agentCount dims params = 
    do
        randCoords <- randomCoords (0,0) dims agentCount

        adefs <- mapM (randomAgentIO params) randCoords
        let occupations = map (\a -> (sugAgCoord $ adState a, (adId a, adState a))) adefs

        initRandomCells <- createCells dims occupations

        let cellsWithSugarLevel1 = initSugar initRandomCells (circlesSugar 1 [((35, 35), 20.0), ((15, 15), 20.0)])
        let cellsWithSugarLevel2 = initSugar cellsWithSugarLevel1 (circlesSugar 2 [((35, 35), 15.0), ((15, 15), 15.0)])
        let cellsWithSugarLevel3 = initSugar cellsWithSugarLevel2 (circlesSugar 3 [((35, 35), 10.0), ((15, 15), 10.0)])
        let cellsWithSugarLevel4 = initSugar cellsWithSugarLevel3 (circlesSugar 4 [((35, 35), 5.0), ((15, 15), 5.0)])

        let cellsWithSpiceLevel1 = initSpice cellsWithSugarLevel4 (circlesSpice 1 [((15, 35), 20.0), ((35, 15), 20.0)])
        let cellsWithSpiceLevel2 = initSpice cellsWithSpiceLevel1 (circlesSpice 2 [((15, 35), 15.0), ((35, 15), 15.0)])
        let cellsWithSpiceLevel3 = initSpice cellsWithSpiceLevel2 (circlesSpice 3 [((15, 35), 10.0), ((35, 15), 10.0)])
        let cellsWithSpiceLevel4 = initSpice cellsWithSpiceLevel3 (circlesSpice 4 [((15, 35), 5.0), ((35, 15), 5.0)])

        rng <- newStdGen

        let e = createDiscrete2d
                    dims
                    neumann
                    WrapBoth
                    cellsWithSpiceLevel4
                    rng

        return (adefs, e)

initSugar :: [(Discrete2dCoord, SugarScapeEnvCell)]
                -> ((Discrete2dCoord, SugarScapeEnvCell) -> Double)
                -> [(Discrete2dCoord, SugarScapeEnvCell)]
initSugar cs sugarFunc = map (initSugarAux sugarFunc) cs
    where
        initSugarAux :: ((Discrete2dCoord, SugarScapeEnvCell) -> Double)
                                -> (Discrete2dCoord, SugarScapeEnvCell)
                                -> (Discrete2dCoord, SugarScapeEnvCell)
        initSugarAux sugarFunc cp@(coord, cell) = (coord, cell')
            where
                sugar = sugarFunc cp
                cell' = cell { sugEnvSugarLevel = sugar,
                                sugEnvSugarCapacity = sugar }

initSpice :: [(Discrete2dCoord, SugarScapeEnvCell)]
                -> ((Discrete2dCoord, SugarScapeEnvCell) -> Double)
                -> [(Discrete2dCoord, SugarScapeEnvCell)]
initSpice cs spiceFunc = map (initSpiceAux spiceFunc) cs
    where
        initSpiceAux :: ((Discrete2dCoord, SugarScapeEnvCell) -> Double)
                                -> (Discrete2dCoord, SugarScapeEnvCell)
                                -> (Discrete2dCoord, SugarScapeEnvCell)
        initSpiceAux spiceFunc cp@(coord, cell) = (coord, cell')
            where
                spice = spiceFunc cp
                cell' = cell { sugEnvSpiceLevel = spice,
                                sugEnvSpiceCapacity = spice }

createCells :: Discrete2dDimension
                -> [(Discrete2dCoord, (AgentId, SugarScapeAgentState))]
                -> IO [(Discrete2dCoord, SugarScapeEnvCell)]
createCells (maxX, maxY) occupations = mapM (initRandomCell occupations) coords
    where
        coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]

initRandomCell :: [(Discrete2dCoord, (AgentId, SugarScapeAgentState))] 
                    -> Discrete2dCoord 
                    -> IO (Discrete2dCoord, SugarScapeEnvCell)
initRandomCell os coord = 
    do
        randSugarCap <- randomRIO sugarCapacityRange
        randSpiceCap <- randomRIO spiceCapacityRange

        let mayOccupier = Data.List.find ((==coord) . fst) os

        let c = SugarScapeEnvCell {
            sugEnvSugarCapacity = 0,
            sugEnvSugarLevel = 0,

            sugEnvSpiceCapacity = 0,
            sugEnvSpiceLevel = 0,

            sugEnvPolutionLevel = 0.0,

            sugEnvOccupier = maybe Nothing (\(_, (aid, s)) -> (Just (cellOccupier aid s))) mayOccupier
        }

        return (coord, c)

-- NOTE: will draw random-coords within (0,0) and limits WITHOUT repeating any coordinate
randomCoords :: Discrete2dDimension -> Discrete2dDimension -> Int -> IO [Discrete2dCoord]
randomCoords lower@(minX, minY) upper@(maxX, maxY) n
    | n > totalCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
    | otherwise = drawRandomCoordsAux lower upper n []
    where
        totalCoords = (maxX - minX) * (maxY - minY)

        -- NOTE: using accumulator, must faster
        drawRandomCoordsAux :: Discrete2dDimension -> Discrete2dDimension -> Int -> [Discrete2dCoord] -> IO [Discrete2dCoord]
        drawRandomCoordsAux _ _ 0 acc = return acc
        drawRandomCoordsAux lower@(minX, minY) upper@(maxX, maxY) n acc =
            do
              randX <- randomRIO (minX, maxX - 1)
              randY <- randomRIO (minY, maxY - 1)

              let c = (randX, randY)
              if elem c acc then
                  drawRandomCoordsAux lower upper n acc
                  else
                    drawRandomCoordsAux lower upper (n-1) (c : acc)

randomAgentIO :: SugarScapeSimParams -> Discrete2dCoord -> IO SugarScapeAgentDef
randomAgentIO params coord = 
    do
        std <- getStdGen
        let aid = newAgentId params

        let (adef, std') = runRand (randomAgent
                                        (aid, coord)
                                        sugarScapeAgentBehaviour
                                        sugarScapeAgentConversation)
                                        std

        return adef

allZeroSugar :: (Discrete2dCoord, SugarScapeEnvCell) -> Double
allZeroSugar _ = 0.0

circlesSugar :: Double -> [(Discrete2dCoord, Double)] -> (Discrete2dCoord, SugarScapeEnvCell) -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise = sugEnvSugarLevel cell -- NOTE: keep the level of before
        where
            withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles

circlesSpice :: Double -> [(Discrete2dCoord, Double)] -> (Discrete2dCoord, SugarScapeEnvCell) -> Double
circlesSpice spiceLevel circles (coord, cell)
    | withinRadius = spiceLevel
    | otherwise = sugEnvSpiceLevel cell -- NOTE: keep the level of before
        where
            withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles