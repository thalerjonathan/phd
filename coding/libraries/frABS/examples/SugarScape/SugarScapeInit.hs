module SugarScape.SugarScapeInit where

import SugarScape.SugarScapeModel
import SugarScape.SugarScapeAgent
import SugarScape.SugarScapeEnvironment

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import Data.List

import System.Random
import Control.Monad.Random

allZeroSugar :: (EnvCoord, SugarScapeEnvCell) -> Double
allZeroSugar _ = 0.0

circlesSugar :: Double -> [(EnvCoord, Double)] -> (EnvCoord, SugarScapeEnvCell) -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise = sugEnvSugarLevel cell -- NOTE: keep the level of before
        where
            withinRadius = any (\(p, r) -> distanceEuclidean p coord <= r) circles

circlesSpice :: Double -> [(EnvCoord, Double)] -> (EnvCoord, SugarScapeEnvCell) -> Double
circlesSpice spiceLevel circles (coord, cell)
    | withinRadius = spiceLevel
    | otherwise = sugEnvSpiceLevel cell -- NOTE: keep the level of before
        where
            withinRadius = any (\(p, r) -> distanceEuclidean p coord <= r) circles

createSugarScape :: Int -> EnvLimits -> IO ([SugarScapeAgentDef], SugarScapeEnvironment)
createSugarScape agentCount l = do
                                    randCoords <- drawRandomCoords (0,0) l agentCount

                                    as <- mapM randomAgentIO (zip [0..agentCount-1] randCoords)
                                    let occupations = map (\a -> (adEnvPos a, (adId a, adState a))) as

                                    initRandomCells <- createCells l occupations

                                    let cellsWithSugarLevel1 = initSugar initRandomCells (circlesSugar 1 [((35, 35), 20.0), ((15, 15), 20.0)])
                                    let cellsWithSugarLevel2 = initSugar cellsWithSugarLevel1 (circlesSugar 2 [((35, 35), 15.0), ((15, 15), 15.0)])
                                    let cellsWithSugarLevel3 = initSugar cellsWithSugarLevel2 (circlesSugar 3 [((35, 35), 10.0), ((15, 15), 10.0)])
                                    let cellsWithSugarLevel4 = initSugar cellsWithSugarLevel3 (circlesSugar 4 [((35, 35), 5.0), ((15, 15), 5.0)])

                                    let cellsWithSpiceLevel1 = initSpice cellsWithSugarLevel4 (circlesSpice 1 [((15, 35), 20.0), ((35, 15), 20.0)])
                                    let cellsWithSpiceLevel2 = initSpice cellsWithSpiceLevel1 (circlesSpice 2 [((15, 35), 15.0), ((35, 15), 15.0)])
                                    let cellsWithSpiceLevel3 = initSpice cellsWithSpiceLevel2 (circlesSpice 3 [((15, 35), 10.0), ((35, 15), 10.0)])
                                    let cellsWithSpiceLevel4 = initSpice cellsWithSpiceLevel3 (circlesSpice 4 [((15, 35), 5.0), ((35, 15), 5.0)])

                                    rng <- newStdGen

                                    let env = createEnvironment
                                                          (Just sugarScapeEnvironmentBehaviour)
                                                          l
                                                          neumann
                                                          WrapBoth
                                                          cellsWithSpiceLevel4
                                                          rng
                                                          Nothing
                                    return (as, env)
                                    --return (trace ("Environment has cells: " ++ (show cells)) (as, env))
    where
        initSugar :: [(EnvCoord, SugarScapeEnvCell)]
                            -> ((EnvCoord, SugarScapeEnvCell) -> Double)
                            -> [(EnvCoord, SugarScapeEnvCell)]
        initSugar cs sugarFunc = map (initSugarAux sugarFunc) cs

            where
                initSugarAux :: ((EnvCoord, SugarScapeEnvCell) -> Double)
                                        -> (EnvCoord, SugarScapeEnvCell)
                                        -> (EnvCoord, SugarScapeEnvCell)
                initSugarAux sugarFunc cp@(coord, cell) = (coord, cell')
                    where
                        sugar = sugarFunc cp
                        cell' = cell { sugEnvSugarLevel = sugar,
                                        sugEnvSugarCapacity = sugar }

        initSpice :: [(EnvCoord, SugarScapeEnvCell)]
                            -> ((EnvCoord, SugarScapeEnvCell) -> Double)
                            -> [(EnvCoord, SugarScapeEnvCell)]
        initSpice cs spiceFunc = map (initSpiceAux spiceFunc) cs

            where
                initSpiceAux :: ((EnvCoord, SugarScapeEnvCell) -> Double)
                                        -> (EnvCoord, SugarScapeEnvCell)
                                        -> (EnvCoord, SugarScapeEnvCell)
                initSpiceAux spiceFunc cp@(coord, cell) = (coord, cell')
                    where
                        spice = spiceFunc cp
                        cell' = cell { sugEnvSpiceLevel = spice,
                                        sugEnvSpiceCapacity = spice }

        createCells :: EnvLimits
                        -> [(EnvCoord, (AgentId, SugarScapeAgentState))]
                        -> IO [(EnvCoord, SugarScapeEnvCell)]
        createCells (maxX, maxY) occupations = do
                                                    let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
                                                    cs <- mapM (randomCell occupations) coords
                                                    return cs

        randomCell :: [(EnvCoord, (AgentId, SugarScapeAgentState))] -> EnvCoord -> IO (EnvCoord, SugarScapeEnvCell)
        randomCell os coord = do
                randSugarCap <- getStdRandom $ randomR sugarCapacityRange
                randSpiceCap <- getStdRandom $ randomR spiceCapacityRange

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
        drawRandomCoords :: EnvLimits -> EnvLimits -> Int -> IO [EnvCoord]
        drawRandomCoords lower@(minX, minY) upper@(maxX, maxY) n
            | n > totalCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
            | otherwise = drawRandomCoordsAux lower upper n []
            where
                totalCoords = (maxX - minX) * (maxY - minY)

                -- NOTE: using accumulator, must faster
                drawRandomCoordsAux :: EnvLimits -> EnvLimits -> Int -> [EnvCoord] -> IO [EnvCoord]
                drawRandomCoordsAux _ _ 0 acc = return acc
                drawRandomCoordsAux lower@(minX, minY) upper@(maxX, maxY) n acc =
                    do
                      randX <- getStdRandom (randomR(minX, maxX - 1))
                      randY <- getStdRandom (randomR(minY, maxY - 1))

                      let c = (randX, randY)
                      if (any (==c) acc) then
                          drawRandomCoordsAux lower upper n acc
                          else
                            drawRandomCoordsAux lower upper (n-1) (c : acc)

        randomAgentIO :: (AgentId, EnvCoord) -> IO SugarScapeAgentDef
        randomAgentIO aidCoord = do
                                    std <- getStdGen
                                    let (adef, std') = runRand (randomAgent
                                                                    aidCoord
                                                                    sugarScapeAgentBehaviour
                                                                    sugarScapeAgentConversation)
                                                                    std
                                    setStdGen std'
                                    return adef
