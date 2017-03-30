module SugarScape.SugarScapeInit where

import SugarScape.SugarScapeModel

import FrABS.Agent.Agent
import FrABS.Env.Environment

import Data.List

import System.Random
import System.IO
import Debug.Trace

createSugarScape :: Int -> EnvLimits -> IO ([SugarScapeAgentDef], SugarScapeEnvironment)
createSugarScape agentCount l = do
                                    randCoords <- drawRandomCoords l agentCount

                                    as <- mapM randomAgent (zip [0..agentCount-1] randCoords)
                                    let occupations = map (\a -> (adEnvPos a, adId a)) as

                                    cells <- createCells l occupations

                                    let env = createEnvironment
                                                          (Just sugarScapeEnvironmentBehaviour)
                                                          l
                                                          neumann
                                                          WrapBoth
                                                          cells
                                    return (as, env)
                                    --return (trace ("Environment has cells: " ++ (show cells)) (as, env))
    where
        createCells :: EnvLimits -> [(EnvCoord, AgentId)] -> IO [(EnvCoord, SugarScapeEnvCell)]
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
                                            sugEnvOccupied = maybe Nothing (Just . snd) mayOccupied
                                        }

                                        return (coord, c)

        -- NOTE: will draw random-coords within (0,0) and limits WITHOUT repeating any coordinate
        drawRandomCoords :: EnvLimits -> Int -> IO [EnvCoord]
        drawRandomCoords l@(maxX, maxY) n
            | n > (maxX * maxY) = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
            | otherwise = drawRandomCoordsAux l n []
            where
                -- NOTE: using accumulator, must faster
                drawRandomCoordsAux :: EnvLimits -> Int -> [EnvCoord] -> IO [EnvCoord]
                drawRandomCoordsAux _ 0 acc = return acc
                drawRandomCoordsAux l@(maxX, maxY) n acc = do
                                                              randX <- getStdRandom (randomR(0, maxX - 1))
                                                              randY <- getStdRandom (randomR(0, maxY - 1))

                                                              let c = (randX, randY)
                                                              if (any (==c) acc) then
                                                                  drawRandomCoordsAux l n acc
                                                                  else
                                                                    drawRandomCoordsAux l (n-1) (c : acc)

        randomAgent :: (Int, EnvCoord) -> IO SugarScapeAgentDef
        randomAgent (agentId, coord) = do
                                        randMeta <- getStdRandom $ randomR metabolismRange
                                        randVision <- getStdRandom $ randomR visionRange
                                        randEnd <- getStdRandom $ randomR sugarEndowmentRange

                                        rng <- newStdGen

                                        let s = SugarScapeAgentState {
                                            sugAgMetabolism = randMeta,
                                            sugAgVision = randVision,
                                            sugAgSugar = randEnd,
                                            sugAgRng = rng
                                        }

                                        let a = AgentDef {
                                            adId = agentId,
                                            adState = s,
                                            adEnvPos = coord,
                                            adBeh = sugarScapeAgentBehaviour }

                                        return a
                                        --return (trace ("Agent " ++ (show agentId) ++ " has state: " ++ (show s) ++ " and coord " ++ (show coord)) a)