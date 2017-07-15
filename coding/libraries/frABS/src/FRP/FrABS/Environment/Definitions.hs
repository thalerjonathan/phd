{-# LANGUAGE MultiParamTypeClasses #-}
module FRP.FrABS.Environment.Definitions (
    EnvironmentWrapping (..),

    Discrete2DDimension,
    Continuous2DDimension,

    Discrete2DCoord,
    Continuous2DCoord,

    Discrete2DLimit,
    Continuous2DLimit,

    Discrete2DNeighbourhood,

    EnvironmentBehaviour,

    Environment (..),
    EnvironmentNetwork (..),
    EnvironmentDiscrete2D (..),
    EnvironmentContinuous2D (..)
  ) where

import FRP.Yampa

-- NOTE: need AgentId
import FRP.FrABS.Agent.Agent

import Data.Graph.Inductive.Graph

import Control.Monad.Trans.State
import Control.Monad.Random

data EnvironmentWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

type Discrete2DDimension = (Int, Int)
type Continuous2DDimension = (Double, Double)

type Discrete2DCoord = Discrete2DDimension
type Continuous2DCoord = Continuous2DDimension

type Discrete2DLimit = Discrete2DDimension
type Continuous2DLimit = Continuous2DDimension

type Discrete2DNeighbourhood = [Discrete2DCoord]

type EnvironmentBehaviour e = SF e e

class Environment e where
    environmentBehaviour :: Maybe (EnvironmentBehaviour e)

class (Environment e) => EnvironmentNetwork e where
    nodesOfNetwork :: e -> [Node]
    networkDegrees :: e -> [(Node, Int)]
    neighbourEdges :: Node -> e ->  [l]
    neighbourNodes :: Node -> e -> [Node]
    neighbourNodesM :: Node -> State e [Node]
    neighbourLinks :: Node -> e -> Adj l
    directLinkBetween :: Node -> Node -> e -> Maybe l
    directLinkBetweenM :: Node -> Node -> State e (Maybe l)



class (Environment e) => EnvironmentSpatial2D e d where
    agentCoord :: (Num d) => AgentId -> e -> (d, d)
    updateAgentCoord :: (Num d) => AgentId -> (d, d) -> e -> e
    environmentDimensions :: e -> (d, d)

-- TODO: how can we make to be Int?
class EnvironmentSpatial2D e d => EnvironmentDiscrete2D e  where
    agentCoordDisc2D :: AgentId -> e -> Discrete2DCoord
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
    
-- TODO: all functions without e are not necessary here
class (EnvironmentSpatial2D e d) => EnvironmentContinuous2D e d where
    agentCoordCont2D :: AgentId -> e -> Continuous2DCoord