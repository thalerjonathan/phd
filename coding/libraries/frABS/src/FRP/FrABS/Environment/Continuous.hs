module FRP.FrABS.Environment.Continuous (
    Continuous2DDimension,
    Continuous2DCoord,

    Continuous2d (..), -- TODO: hide data-constructor
    -- Continuous2dNetwork (..),
    
    createContinuous2d,
    
    environmentDimensionsCont2D,

    distanceManhattanCont2D,
    distanceEuclideanCont2D,

    wrapCont2D,
    wrapCont2DEnv,

    multCoord,
    addCoord,
    subCoord,
    vecFromCoord,
    vecLen,
    vecNorm
  ) where

import FRP.FrABS.Environment.Spatial

import Control.Monad.Random

{-
class EnvCont2d e where
    agentCoordCont2D :: AgentId -> e -> Continuous2DCoord
    updateAgentCoordCont2D :: AgentId -> Continuous2DCoord -> e -> e
    environmentDimensionsCont2D :: e -> Continuous2DCoord
-}

type Continuous2DDimension = (Double, Double)
type Continuous2DCoord = Continuous2DDimension

data Continuous2d = Continuous2d {
    envCont2dDims :: Continuous2DDimension,
    envCont2dWrapping :: EnvironmentWrapping
}

{-
import FRP.FrABS.Environment.Network

data Continuous2dNetwork l = Continuous2dNetwork {
    envCombCont2dNetwork :: Network l,
    envCombCont2d :: Continuous2d
}
-}

createContinuous2d :: Continuous2DDimension
                        -> EnvironmentWrapping
                        -> Continuous2d
createContinuous2d d w = Continuous2d {
                            envCont2dDims = d,
                            envCont2dWrapping = w }

environmentDimensionsCont2D :: Continuous2d -> Continuous2DCoord
environmentDimensionsCont2D e = envCont2dDims e

distanceManhattanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceManhattanCont2D (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceEuclideanCont2D (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = x2 - x1
        yDelta = y2 - y1

multCoord :: Double -> Continuous2DCoord -> Continuous2DCoord
multCoord s (x, y) = (x*s, y*s)

addCoord :: Continuous2DCoord -> Continuous2DCoord -> Continuous2DCoord
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subCoord :: Continuous2DCoord -> Continuous2DCoord -> Continuous2DCoord
subCoord (x1, y1) (x2, y2) = (x1-x2, y1-y2)

vecFromCoord :: Continuous2DCoord -> Continuous2DCoord -> Continuous2DCoord
vecFromCoord (x1, y1) (x2, y2) = (x2-x1, y2-y1)

vecLen :: Continuous2DCoord -> Double
vecLen (x, y) = sqrt( x * x + y * y )

vecNorm :: Continuous2DCoord -> Continuous2DCoord
vecNorm (x, y)
    | len == 0 = (0, 0)
    | otherwise = (x / len, y / len)
    where
        len = vecLen (x, y)

wrapCont2DEnv :: Continuous2d -> Continuous2DCoord -> Continuous2DCoord
wrapCont2DEnv e c = wrapCont2D d w c
    where
        d = envCont2dDims e
        w = envCont2dWrapping e

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