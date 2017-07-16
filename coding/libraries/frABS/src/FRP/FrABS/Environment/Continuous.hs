module FRP.FrABS.Environment.Continuous (
    Continuous2DEnvironment(..),

    EnvCont2d (..),

    distanceManhattanCont2D,
    distanceEuclideanCont2D,

    wrapCont2D
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Definitions

import Data.List
import Data.Array.IArray
import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Data.Map as Map

data Continuous2DEnvironment = Continuous2DEnvironment {
    envCont2dDims :: Continuous2DDimension,
    envCont2dWrapping :: EnvironmentWrapping,
    envCont2dRng :: StdGen,
    envCont2dAgentPositions :: Map.Map AgentId Continuous2DCoord
}

instance EnvCont2d Continuous2DEnvironment where
    -- agentCoordCont2D :: AgentId -> e -> Continuous2DCoord
    agentCoordCont2D aid e = (0, 0) -- TODO: implement

    -- updateAgentCoordCont2D :: AgentId -> Continuous2DCoord -> e -> e
    updateAgentCoordCont2D aid coord e = e -- TODO: implement

    -- environmentDimensionsCont2D :: e -> Continuous2DCoord
    environmentDimensionsCont2D e = envCont2dDims e

distanceManhattanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceManhattanCont2D (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceEuclideanCont2D (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = x2 - x1
        yDelta = y2 - y1

wrapCont2D :: Continuous2DDimension -> EnvironmentWrapping -> Continuous2DCoord -> Continuous2DCoord
wrapCont2D (maxX, maxY) ClipToMax (x, y) = (max 0 (min x maxX), max 0 (min y maxY))
wrapCont2D l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrapCont2D l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrapCont2D l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrapCont2D l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrapCont2D l WrapVertical (x, y + maxY)
    | y >= maxY = wrapCont2D l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrapCont2D l WrapBoth coord = wrapCont2D l WrapHorizontal $ wrapCont2D l WrapVertical coord