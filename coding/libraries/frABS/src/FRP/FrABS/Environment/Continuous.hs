module FRP.FrABS.Environment.Continuous 
  (
    Continuous2dDimension
  , Continuous2dCoord

  , Continuous2d (..)

  , Continuous2dEmpty
  , DistanceFunction

  , createContinuous2d

  , stepTo
  , stepRandom

  , distanceManhattanCont2d
  , distanceEuclideanCont2d

  , wrapCont2d
  , wrapCont2dEnv

  , multCoord
  , addCoord
  , subCoord
  , vecFromCoords
  , vecLen
  , vecNorm
  , dotCoords

  , objectPresent
  , objectCoord
  , removeObject
  , updateObject
  , objectsInDistance
  ) where

import Control.Monad.Random
import qualified Data.Map as Map

import FRP.FrABS.Environment.Spatial

type Continuous2dDimension  = (Double, Double)
type Continuous2dCoord      = Continuous2dDimension

type Continuous2dEmpty      = Continuous2d ()
type DistanceFunction       = (Continuous2dCoord -> Double)

data Continuous2d o = Continuous2d 
  {
    envCont2dDims       :: Continuous2dDimension
  , envCont2dWrapping   :: EnvironmentWrapping
  , envCont2dObjects    :: Map.Map o Continuous2dCoord
  }

createContinuous2d :: Continuous2dDimension
                        -> EnvironmentWrapping
                        -> Continuous2d o
createContinuous2d d w = 
  Continuous2d {
    envCont2dDims = d
  , envCont2dWrapping = w
  , envCont2dObjects = Map.empty 
  }

stepTo :: Continuous2d o -> Double -> Continuous2dCoord -> Continuous2dCoord -> Continuous2dCoord
stepTo e step from to = wrapCont2dEnv e from'
  where
    dir = vecNorm $ vecFromCoords from to
    from' = addCoord from (multCoord step dir)

stepRandom :: RandomGen g =>
              Continuous2dCoord 
              -> Continuous2d o
              -> Double 
              -> Rand g Continuous2dCoord
stepRandom (ox, oy) e step =  do
  --randAngle <- getRandomR ((0, 360) :: (Double, Double))
  --let rad = randAngle * (pi / 180) 

  randRadians <- getRandomR (0, 2*pi)

  let x = cos randRadians
  let y = sin randRadians

  let ox' = ox + x * step
  let oy' = oy + y * step

  return $ wrapCont2dEnv e (ox', oy')

distanceManhattanCont2d :: Continuous2dCoord -> DistanceFunction
distanceManhattanCont2d (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanCont2d :: Continuous2dCoord -> DistanceFunction
distanceEuclideanCont2d (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
  where
    xDelta = x2 - x1
    yDelta = y2 - y1

multCoord :: Double -> Continuous2dCoord -> Continuous2dCoord
multCoord s (x, y) = (x*s, y*s)

addCoord :: Continuous2dCoord -> Continuous2dCoord -> Continuous2dCoord
addCoord (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subCoord :: Continuous2dCoord -> Continuous2dCoord -> Continuous2dCoord
subCoord (x1, y1) (x2, y2) = (x1-x2, y1-y2)

vecFromCoords :: Continuous2dCoord -> Continuous2dCoord -> Continuous2dCoord
vecFromCoords (x1, y1) (x2, y2) = (x2-x1, y2-y1)

vecLen :: Continuous2dCoord -> Double
vecLen (x, y) = sqrt( x * x + y * y )

dotCoords :: Continuous2dCoord -> Continuous2dCoord -> Double
dotCoords (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

vecNorm :: Continuous2dCoord -> Continuous2dCoord
vecNorm (x, y)
    | len == 0 = (0, 0)
    | otherwise = (x / len, y / len)
  where
      len = vecLen (x, y)

wrapCont2dEnv :: Continuous2d o -> Continuous2dCoord -> Continuous2dCoord
wrapCont2dEnv e c = wrapCont2d d w c
  where
    d = envCont2dDims e
    w = envCont2dWrapping e

wrapCont2d :: Continuous2dDimension -> EnvironmentWrapping -> Continuous2dCoord -> Continuous2dCoord
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

-------------------------------------------------------------------------------
-- OBJECT-Management
-------------------------------------------------------------------------------
objectPresent :: (Ord o) => o -> Continuous2d o -> Bool
objectPresent o e = Map.member o objs
  where
    objs = envCont2dObjects e

objectCoord :: (Ord o) => o -> Continuous2d o -> Maybe Continuous2dCoord
objectCoord o e = Map.lookup o objs
  where
    objs = envCont2dObjects e

removeObject :: (Ord o) => o -> Continuous2d o -> Continuous2d o
removeObject o e = e { envCont2dObjects = objs' }
  where
    objs = envCont2dObjects e
    objs' = Map.delete o objs

updateObject :: (Ord o) => o -> Continuous2dCoord -> Continuous2d o -> Continuous2d o
updateObject o coord e = e { envCont2dObjects = objs' }
  where
    objs = envCont2dObjects e 
    objs' = Map.insert o coord objs

-- NOTE: use currying to fix the origin in the distance-function e.g. distanceManhattanCont2d or distanceEuclideanCont2d
objectsInDistance :: (Ord o) => Double 
                                -> DistanceFunction
                                -> Continuous2d o
                                -> [o]
objectsInDistance d df e = Map.foldrWithKey objectsInDistanceAux [] objs
  where
    objs = envCont2dObjects e 

    objectsInDistanceAux :: o -> Continuous2dCoord -> [o] -> [o]
    objectsInDistanceAux o coord accObjs
      | df coord <= d = o : accObjs
      | otherwise = accObjs
-------------------------------------------------------------------------------