module SugarScape.SugarScapeInit where

import SugarScape.SugarScapeModel
import SugarScape.SugarScapeEnvironment
import SugarScape.SugarScapeAgent

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import Data.List

import System.Random
import System.IO
import Debug.Trace

allZeroSugar :: (EnvCoord, SugarScapeEnvCell) -> Double
allZeroSugar _ = 0.0

circlesSugar :: Double -> [(EnvCoord, Double)] -> (EnvCoord, SugarScapeEnvCell) -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise = sugEnvSugarLevel cell -- NOTE: keep the level of before
        where
            withinRadius = any (\(p, r) -> distanceEucl p coord <= r) circles

createSugarScape :: Int -> EnvLimits -> IO ([SugarScapeAgentDef], SugarScapeEnvironment)
createSugarScape agentCount l = do
                                    randCoords <- drawRandomCoords (0,0) l agentCount

                                    as <- mapM randomAgentIO (zip [0..agentCount-1] randCoords)
                                    let occupations = map (\a -> (adEnvPos a, adId a)) as

                                    initRandomSugarCells <- createCells l occupations

                                    let zeroSugarCells = initSugar initRandomSugarCells allZeroSugar
                                    let cellsWithLevel1 = initSugar zeroSugarCells (circlesSugar 1 [((35, 35), 20.0), ((15, 15), 20.0)])
                                    let cellsWithLevel2 = initSugar cellsWithLevel1 (circlesSugar 2 [((35, 35), 15.0), ((15, 15), 15.0)])
                                    let cellsWithLevel3 = initSugar cellsWithLevel2 (circlesSugar 3 [((35, 35), 10.0), ((15, 15), 10.0)])
                                    let cellsWithLevel4 = initSugar cellsWithLevel3 (circlesSugar 4 [((35, 35), 5.0), ((15, 15), 5.0)])

                                    let env = createEnvironment
                                                          (Just sugarScapeEnvironmentBehaviour)
                                                          l
                                                          neumann
                                                          WrapBoth
                                                          cellsWithLevel4
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

        createCells :: EnvLimits
                        -> [(EnvCoord, AgentId)]
                        -> IO [(EnvCoord, SugarScapeEnvCell)]
        createCells (maxX, maxY) occupations = do
                                                    let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
                                                    let cellCount = maxX * maxY
                                                    cs <- mapM (randomCell occupations) coords
                                                    return cs

        randomCell :: [(EnvCoord, AgentId)] -> EnvCoord -> IO (EnvCoord, SugarScapeEnvCell)
        randomCell occupations coord = do
                                        randCap <- getStdRandom $ randomR sugarCapacityRange

                                        let mayOccupied = Data.List.find ((==coord) . fst) occupations

                                        let c = SugarScapeEnvCell {
                                            sugEnvSugarCapacity = randCap,
                                            sugEnvSugarLevel = randCap,
                                            sugEnvPolutionLevel = 0.0,
                                            sugEnvOccupied = maybe Nothing (Just . snd) mayOccupied
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
                                    let (adef, std') = randomAgent aidCoord std
                                    setStdGen std'
                                    return adef