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

initAgentZeroEpstein :: IO ([AgentZeroAgentDef], AgentZeroEnvironment)
initAgentZeroEpstein = 
  do
    let dims = (50, 50)

    a0 <- runCreateAgentZeroIO (0, (16, 34)) [(1, 0.3), (2, 0.3)]
    a1 <- runCreateAgentZeroIO (1, (30, 20)) [(0, 0.3), (2, 0.3)]
    a2 <- runCreateAgentZeroIO (2, (28, 16)) [(0, 0.3), (1, 0.3)]

    cells <- createCells dims
    envRng <- newStdGen 

    let env = createEnvironment
                          (Just agentZeroEnvironmentBehaviour)
                          dims
                          neumann
                          WrapBoth
                          cells
                          envRng

    return ([a0, a1, a2], env)

  where
    runCreateAgentZeroIO :: (AgentId, EnvCoord) 
                      -> [(AgentId, Double)]
                      -> IO AgentZeroAgentDef
    runCreateAgentZeroIO aidCoord cs = 
      do
        std <- getStdGen
        let (adef, std') = runRand (createAgentZero
                                        aidCoord
                                        cs
                                        agentZeroAgentBehaviour)
                                        std
        setStdGen std'
        return adef

initAgentZeroCount :: Int -> EnvLimits -> IO ([AgentZeroAgentDef], AgentZeroEnvironment)
initAgentZeroCount agentCount l = 
  do
    randCoords <- drawRandomCoords (0,0) l agentCount

    as <- mapM runCreateAgentZeroIO (zip [0..agentCount-1] randCoords)
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

  where
    runCreateAgentZeroIO :: (AgentId, EnvCoord) 
                      -> IO AgentZeroAgentDef
    runCreateAgentZeroIO aidCoord = 
      do
        std <- getStdGen
        let (adef, std') = runRand (createAgentZero
                                        aidCoord
                                        []
                                        agentZeroAgentBehaviour)
                                        std
        setStdGen std'
        return adef

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

createCells :: EnvLimits
                -> IO [(EnvCoord, AgentZeroEnvCell)]
createCells (maxX, maxY) = 
  do
    let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
    cs <- mapM randomCell coords
    return cs
  where
    randomCell :: EnvCoord -> IO (EnvCoord, AgentZeroEnvCell)
    randomCell coord = do
            randShade <- getStdRandom $ randomR (0.0, 0.75)

            let c = AgentZeroEnvCell {
                azCellState = Friendly,
                azCellShade = randShade
            }

            return (coord, c)

