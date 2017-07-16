module FRP.FrABS.Environment.Definitions (
    EnvironmentWrapping (..),
    EnvironmentBehaviour,
    EnvironmentCollapsing
  ) where

import FRP.Yampa

data EnvironmentWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

-- newtype EnvironmentSpatial2DDimension d = (Num d, Num d)

type EnvironmentBehaviour e = SF e e
type EnvironmentCollapsing e = ([e] -> e)

{-
class EnvNet e where
    nodesOfNetwork :: e -> [AgentId]
    networkDegrees :: e -> [(AgentId, Int)]
    neighbourEdges :: AgentId -> e ->  [l]
    neighbourNodes :: AgentId -> e -> [AgentId]
    neighbourNodesM :: AgentId -> State e [AgentId]
    neighbourLinks :: AgentId -> e -> Adj l
    directLinkBetween :: AgentId -> AgentId -> e -> Maybe l
    directLinkBetweenM :: AgentId -> AgentId -> State e (Maybe l)

class EnvSpatial2d e d where
    agentCoord :: (Num d) => AgentId -> e -> (d, d)
    updateAgentCoord :: (Num d) => AgentId -> (d, d) -> e -> e
    environmentDimensions :: (Num d) => e -> (d, d)

class EnvDisc2d e c where
    -- agentCoordDisc2D :: AgentId -> e -> Discrete2DCoord
    -- updateAgentCoordDisc2D :: AgentId -> Discrete2DCoord -> e -> e
    -- environmentDimensionsDisc2D :: e -> Discrete2DDimension
    allCellsWithCoords :: e -> [(Discrete2DCoord, c)]
    updateEnvironmentCells :: (c -> c) -> e-> e
    updateEnvironmentCellsWithCoords :: ((Discrete2DCoord, c) -> c) -> e -> e
    changeCellAt :: Discrete2DCoord -> c -> e -> e
    changeCellAtM :: Discrete2DCoord -> c -> State e ()
    cellsAroundRadius :: Discrete2DCoord -> Double -> e -> [(Discrete2DCoord, c)]
    cellsAroundRadiusM :: Discrete2DCoord -> Double -> State e [(Discrete2DCoord, c)]
    cellsAroundRect :: Discrete2DCoord -> Int -> e -> [(Discrete2DCoord, c)]
    cellsAt :: [Discrete2DCoord] -> e -> [c]
    cellAt :: Discrete2DCoord -> e -> c
    cellAtM :: Discrete2DCoord -> State e c 
    randomCell :: e -> Rand StdGen (c, Discrete2DCoord)
    randomCellWithinRect :: Discrete2DCoord -> Int -> e -> Rand StdGen (c, Discrete2DCoord)
    neighboursDistance :: Discrete2DCoord -> Int -> e -> [(Discrete2DCoord, c)]
    neighboursDistanceM :: Discrete2DCoord -> Int -> State e [(Discrete2DCoord, c)]
    neighbours :: Discrete2DCoord -> e -> [(Discrete2DCoord, c)]
    neighboursM :: Discrete2DCoord -> State e [(Discrete2DCoord, c)]

class EnvCont2d e where
    agentCoordCont2D :: AgentId -> e -> Continuous2DCoord
    updateAgentCoordCont2D :: AgentId -> Continuous2DCoord -> e -> e
    environmentDimensionsCont2D :: e -> Continuous2DCoord
-}