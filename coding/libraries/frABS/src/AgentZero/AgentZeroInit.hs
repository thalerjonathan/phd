module AgentZero.AgentZeroInit where

import AgentZero.AgentZeroModel
import AgentZero.AgentZeroAgent
import AgentZero.AgentZeroEnvironment

import FrABS.Agent.Agent
import FrABS.Env.Environment


import FRP.Yampa

import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import System.Random
import Control.Monad.Random

initAgentZeroEpstein :: IO ([AgentZeroAgentDef], AgentZeroEnvironment)
initAgentZeroEpstein = 
  do
    let dims = (33, 33)

    let a0Node = 0
    let a1Node = 1
    let a2Node = 2
    
    let a0LNode = (a0Node, ()) :: LNode ()
    let a1LNode = (a1Node, ()) :: LNode ()
    let a2LNode = (a2Node, ()) :: LNode ()
    
    let a0_a1LEdge = (a0Node, a1Node, 0.3) :: LEdge Double
    let a0_a2LEdge = (a0Node, a2Node, 0.3) :: LEdge Double
    
    let a1_a0LEdge = (a1Node, a0Node, 0.3) :: LEdge Double
    let a1_a2LEdge = (a1Node, a2Node, 0.3) :: LEdge Double
    
    let a2_a0LEdge = (a2Node, a0Node, 0.3) :: LEdge Double
    let a2_a1LEdge = (a2Node, a1Node, 0.3) :: LEdge Double
    
    let gr = mkGraph [a0LNode, a1LNode, a2LNode] [a0_a1LEdge, a0_a2LEdge, a1_a2LEdge] :: Gr () Double

    let nsA0 = lneighbors gr a0Node
    let nsA1 = lneighbors gr a1Node
    let nsA2 = lneighbors gr a2Node
    putStrLn (show nsA0)
    putStrLn (show nsA1)
    putStrLn (show nsA2)

    a0 <- runCreateAgentZeroIO (0, (9, 9))
    a1 <- runCreateAgentZeroIO (1, (22, 17))
    a2 <- runCreateAgentZeroIO (2, (20, 26))

    cells <- createCells dims
    envRng <- newStdGen 

    let env = createEnvironment
                          (Just agentZeroEnvironmentBehaviour)
                          dims
                          neumann
                          ClipToMax
                          cells
                          envRng
                          (Just gr)

    return ([a0, a1, a2], env)

  where
    

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
                          Nothing
    return (as, env)

  where
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

runCreateAgentZeroIO :: (AgentId, EnvCoord) 
                      -> IO AgentZeroAgentDef
runCreateAgentZeroIO aidCoord = 
  do
    std <- getStdGen
    let (adef, std') = runRand (createAgentZero
                                    aidCoord
                                    agentZeroAgentBehaviour)
                                    std
    setStdGen std'
    return adef

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

