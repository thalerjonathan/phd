module SugarScape.SugarScapeInit where

import SugarScape.SugarScapeModel

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random
import System.IO
import Debug.Trace

createSugarScape :: Int -> EnvLimits -> IO ([SugarScapeAgentDef], SugarScapeEnvironment)
createSugarScape agentCount l = do
                                    cells <- createCells l
                                    randCoords <- drawRandomCoords l agentCount
                                    as <- mapM randomAgent (zip [0..agentCount-1] randCoords)

                                    let env = createEnvironment
                                                          (Just sugarScapeEnvironmentBehaviour)
                                                          l
                                                          neumann
                                                          WrapBoth
                                                          cells
                                    return (as, env)
    where
        createCells :: EnvLimits -> IO [(EnvCoord, SugarScapeEnvCell)]
        createCells (maxX, maxY) = do
                                        let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
                                        let cellCount = maxX * maxY
                                        cs <- mapM randomCell coords
                                        return cs

        randomCell :: EnvCoord -> IO (EnvCoord, SugarScapeEnvCell)
        randomCell coord = do
                                cap <- getStdRandom (randomR(sugarMinCapacity, sugarMaxCapacity))

                                let c = SugarScapeEnvCell {
                                    sugEnvSugarCapacity = cap,
                                    sugEnvSugarLevel = cap
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
                                        rng <- newStdGen

                                        let s = SugarScapeAgentState {
                                            sugTribe = 0,
                                            sugRng = rng
                                        }

                                        let a = AgentDef {
                                            adId = agentId,
                                            adState = s,
                                            adEnvPos = coord,
                                            adBeh = sugarScapeAgentBehaviour }

                                        return a
                                        --return (trace ("Agent " ++ (show agentId) ++ " has coord " ++ (show coord)) a)