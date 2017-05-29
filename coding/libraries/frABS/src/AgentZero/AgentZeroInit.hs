module AgentZero.AgentZeroInit where

import AgentZero.AgentZeroModel
import AgentZero.AgentZeroAgent
import AgentZero.AgentZeroEnvironment

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import Data.List

import System.Random
import Control.Monad.Random

createAgentZero :: Int -> EnvLimits -> IO ([AgentZeroAgentDef], AgentZeroEnvironment)
createAgentZero agentCount l = do
                                    randCoords <- drawRandomCoords (0,0) l agentCount

                                    as <- mapM randomAgentIO (zip [0..agentCount-1] randCoords)
                                    initRandomCells <- createCells l
                                    envRng <- newStdGen 

                                    let env = createEnvironment
                                                          (Just agentZeroEnvironmentBehaviour)
                                                          l
                                                          neumann
                                                          WrapBoth
                                                          initRandomCells
                                                          envRng
                                    return (as, env)
                                    --return (trace ("Environment has cells: " ++ (show cells)) (as, env))
    where
        createCells :: EnvLimits
                        -> IO [(EnvCoord, AgentZeroEnvCell)]
        createCells (maxX, maxY) = do
                                      let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
                                      cs <- mapM randomCell coords
                                      return cs

        randomCell :: EnvCoord -> IO (EnvCoord, AgentZeroEnvCell)
        randomCell coord = do
                randShade <- getStdRandom $ randomR (0.0, 0.75)

                let c = AgentZeroEnvCell {
                    azCellState = Friendly,
                    azCellShade = randShade
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

        randomAgentIO :: (AgentId, EnvCoord) -> IO AgentZeroAgentDef
        randomAgentIO aidCoord = do
                                    std <- getStdGen
                                    let (adef, std') = runRand (randomAgent
                                                                    aidCoord
                                                                    agentZeroAgentBehaviour)
                                                                    std
                                    setStdGen std'
                                    return adef
