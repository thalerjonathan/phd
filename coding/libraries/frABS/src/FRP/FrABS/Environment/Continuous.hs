module FRP.FrABS.Environment.Continuous (
    Continuous2DDimension,
    Continuous2DCoord,

    Continuous2d (..), -- TODO: hide data-constructor
    
    createContinuous2d,
    
    stepTo,
    stepRandom,

    distanceManhattanCont2D,
    distanceEuclideanCont2D,

    wrapCont2d,
    wrapCont2dEnv,

    multCoord,
    addCoord,
    subCoord,
    vecFromCoord,
    vecLen,
    vecNorm
  ) where

import FRP.FrABS.Environment.Spatial

import Control.Monad.Random

type Continuous2DDimension = (Double, Double)
type Continuous2DCoord = Continuous2DDimension

data Continuous2d = Continuous2d {
    envCont2dDims :: Continuous2DDimension,
    envCont2dWrapping :: EnvironmentWrapping
}

createContinuous2d :: Continuous2DDimension
                        -> EnvironmentWrapping
                        -> Continuous2d
createContinuous2d d w = Continuous2d {
                            envCont2dDims = d,
                            envCont2dWrapping = w }

stepTo :: Continuous2d -> Double -> Continuous2DCoord -> Continuous2DCoord -> Continuous2DCoord
stepTo e step from to = wrapCont2dEnv e from'
    where
        dir = vecNorm $ vecFromCoord from to
        from' = addCoord from (multCoord step dir)

stepRandom :: Continuous2DCoord 
                -> Continuous2d 
                -> Double 
                -> Rand StdGen Continuous2DCoord
stepRandom (ox, oy) e step =  
    do
        --randAngle <- getRandomR ((0, 360) :: (Double, Double))
        --let rad = randAngle * (pi / 180) 

        randRadians <- getRandomR (0, 2*pi)

        let x = cos randRadians
        let y = sin randRadians

        let ox' = ox + x * step
        let oy' = oy + y * step

        return $ wrapCont2dEnv e (ox', oy')

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

wrapCont2dEnv :: Continuous2d -> Continuous2DCoord -> Continuous2DCoord
wrapCont2dEnv e c = wrapCont2d d w c
    where
        d = envCont2dDims e
        w = envCont2dWrapping e

wrapCont2d :: Continuous2DDimension -> EnvironmentWrapping -> Continuous2DCoord -> Continuous2DCoord
wrapCont2d (maxX, maxY) ClipToMax (x, y) = (max 0 (min x maxX), max 0 (min y maxY))
wrapCont2d l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrapCont2d l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrapCont2d l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrapCont2d l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrapCont2d l WrapVertical (x, y + maxY)
    | y >= maxY = wrapCont2d l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrapCont2d l WrapBoth coord = wrapCont2d l WrapHorizontal $ wrapCont2d l WrapVertical coord