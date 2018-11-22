-- NOTE: if this is NOT strict, then memory builds up like HELL
{-# LANGUAGE Strict           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Discrete 
  ( Discrete2dDimension
  , Discrete2dCoord
  , Discrete2dNeighbourhood
  , Discrete2dCell

  , Discrete2d (..)

  , EnvironmentWrapping (..)
  
  , createDiscrete2d

  , dimensionsDisc2d

  , allCells
  , allCellsWithCoords
  , updateCells
  , updateCellsWithCoords
  , updateCellAt
  , changeCellAt
  , cellsAt
  , cellAt

  , neighbours
  , neighbourCells

  , neighboursInNeumannDistance
  , neighboursCellsInNeumannDistance

  , distanceManhattanDisc2d
  , distanceEuclideanDisc2d
  , neighbourhoodOf
  , neighbourhoodScale
  , wrapCells
  , neumann
  , moore
  , wrapNeighbourhood
  , wrapDisc2d
  ) where

import Control.Concurrent.STM
import Control.Monad.Random
import Data.Array.MArray

type Discrete2dDimension     = (Int, Int)
type Discrete2dCoord         = Discrete2dDimension
type Discrete2dNeighbourhood = [Discrete2dCoord]
type Discrete2dCell c        = (Discrete2dCoord, c)

data EnvironmentWrapping 
  = ClipToMax 
  | WrapHorizontal 
  | WrapVertical 
  | WrapBoth
  deriving (Show, Read, Eq)

data Discrete2d c = Discrete2d 
  { envDisc2dNeighbourhood :: Discrete2dNeighbourhood
  , envDisc2dWrapping      :: EnvironmentWrapping
  , envDisc2dCells         :: TArray Discrete2dCoord c
  }

createDiscrete2d :: Discrete2dDimension
                 -> Discrete2dNeighbourhood
                 -> EnvironmentWrapping
                 -> [Discrete2dCell c]
                 -> STM (Discrete2d c)
createDiscrete2d (xLimit, yLimit) n w cs = do
  arr <- newArray_ ((0, 0), (xLimit - 1, yLimit - 1))

  forM_ cs (uncurry $ writeArray arr) 

  return Discrete2d {
    envDisc2dNeighbourhood = n
  , envDisc2dWrapping      = w
  , envDisc2dCells         = arr
  }

dimensionsDisc2d :: Discrete2d c -> STM Discrete2dDimension
dimensionsDisc2d e = snd <$> getBounds (envDisc2dCells e)

allCells :: Discrete2d c -> STM [c]
allCells e = getElems $ envDisc2dCells e

allCellsWithCoords :: Discrete2d c -> STM [Discrete2dCell c]
allCellsWithCoords e = getAssocs $ envDisc2dCells e

updateCells :: (c -> c) 
            -> Discrete2d c 
            -> STM ()
updateCells f e = do
    cs <- allCellsWithCoords e
    -- NOTE: can't use mapArray because creates a new array
    mapM_ forceStrict cs
  where
    arr = envDisc2dCells e

    -- NOTE: by using this function and {-# LANGUAGE Strict #-}
    -- we were able to remove a serious memory-leak 
    -- The crucial part is that we make c' explicit which seems
    -- to force the result as well, just implementing as
    -- forceStrict (coord, c) = writeArray arr coord (f $! c) STILL LEAKS!
    forceStrict (coord, c) = writeArray arr coord c'
      where
        -- crucial bit is here
        c' = f c

updateCellsWithCoords :: (Discrete2dCell c -> c) 
                      -> Discrete2d c 
                      -> STM ()
updateCellsWithCoords f e = do
    ecs <- allCellsWithCoords e
    
    let cs       = map f ecs
        ecCoords = map fst ecs

    mapM_ forceStrict (zip ecCoords cs)
  where
    -- NOTE: by using this function and {-# LANGUAGE Strict #-}
    -- we were able to remove a serious memory-leak 
    -- The crucial part is that we make c' explicit which seems
    -- to force the result as well, just implementing as
    -- forceStrict (coord, c) = changeCellAt coord (f $! cc) e STILL LEAKS!
    forceStrict cc@(coord, _) = changeCellAt coord c' e
      where
        -- crucial bit is here
        c' = f cc

updateCellAt :: Discrete2dCoord 
             -> (c -> c) 
             -> Discrete2d c 
             -> STM ()
updateCellAt coord f e = do
    c <- f <$> readArray arr coord
    writeArray arr coord c
  where
    arr = envDisc2dCells e

changeCellAt :: Discrete2dCoord 
             -> c 
             -> Discrete2d c 
             -> STM ()
changeCellAt coord c e = do
  let arr = envDisc2dCells e
  writeArray arr coord c

cellsAt :: [Discrete2dCoord] 
        -> Discrete2d c 
        -> STM [c]
cellsAt cs e = do
  let arr = envDisc2dCells e
  mapM (readArray arr) cs

cellAt :: Discrete2dCoord 
       -> Discrete2d c 
       -> STM c
cellAt coord e = do
  let arr = envDisc2dCells e
  readArray arr coord

  -- NOTE: this function does only work for neumann-neighbourhood, it ignores
--       the environments neighbourhood. also it does not include the coord itself
neighboursInNeumannDistance :: Discrete2dCoord 
                            -> Int 
                            -> Bool 
                            -> Discrete2d c 
                            -> STM [Discrete2dCell c]
neighboursInNeumannDistance coord dist ic e = do
    l <- dimensionsDisc2d e

    let wrappedNs = wrapNeighbourhood l w ns

    cells <- cellsAt wrappedNs e
    return $ zip wrappedNs cells
  where
    n           = neumann
    coordDeltas = foldr (\v acc -> acc ++ neighbourhoodScale n v) [] [1 .. dist]
    w           = envDisc2dWrapping e
    ns          = neighbourhoodOf coord ic coordDeltas

neighboursCellsInNeumannDistance :: Discrete2dCoord 
                                 -> Int 
                                 -> Bool 
                                 -> Discrete2d c 
                                 -> STM [c]
neighboursCellsInNeumannDistance coord dist ic e = do
  ns <- neighboursInNeumannDistance coord dist ic e
  return $ map snd ns

neighbours :: Discrete2dCoord 
           -> Bool 
           -> Discrete2d c 
           -> STM [Discrete2dCell c]
neighbours coord ic e = do
    l <- dimensionsDisc2d e
    let wrappedNs = wrapNeighbourhood l w ns
    cells <- cellsAt wrappedNs e
    return $ zip wrappedNs cells
  where
    n  = envDisc2dNeighbourhood e
    w  = envDisc2dWrapping e
    ns = neighbourhoodOf coord ic n
    
neighbourCells :: Discrete2dCoord 
               -> Bool 
               -> Discrete2d c 
               -> STM [c]
neighbourCells coord ic e = do
  ns <- neighbours coord ic e
  return $ map snd ns 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
distanceManhattanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Int
distanceManhattanDisc2d (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distanceEuclideanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Double
distanceEuclideanDisc2d (x1, y1) (x2, y2) = sqrt (xd*xd + yd*yd)
  where
    xd = fromRational $ toRational (x2 - x1)
    yd = fromRational $ toRational (y2 - y1)

neighbourhoodOf :: Discrete2dCoord 
                -> Bool 
                -> Discrete2dNeighbourhood 
                -> Discrete2dNeighbourhood
neighbourhoodOf coord@(x,y) includeCoord ns 
    | includeCoord = coord : ns'
    | otherwise = ns'
  where
    ns' = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: Discrete2dNeighbourhood -> Int -> Discrete2dNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: Discrete2dDimension 
          -> EnvironmentWrapping 
          -> Discrete2dNeighbourhood 
          -> Discrete2dNeighbourhood
wrapCells = wrapNeighbourhood

neumann :: Discrete2dNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]

moore :: Discrete2dNeighbourhood
moore = [ topLeftDelta,    topDelta,     topRightDelta,
          leftDelta,                     rightDelta,
          bottomLeftDelta, bottomDelta,  bottomRightDelta ]

wrapNeighbourhood :: Discrete2dDimension 
                  -> EnvironmentWrapping 
                  -> Discrete2dNeighbourhood 
                  -> Discrete2dNeighbourhood
wrapNeighbourhood l w = map (wrapDisc2d l w)

wrapDisc2d :: Discrete2dDimension 
           -> EnvironmentWrapping 
           -> Discrete2dCoord 
           -> Discrete2dCoord
wrapDisc2d (maxX, maxY) ClipToMax (x, y) = 
                  (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1)))
wrapDisc2d l@(maxX, _) WrapHorizontal (x, y)
  | x < 0       = wrapDisc2d l WrapHorizontal (x + maxX, y)
  | x >= maxX   = wrapDisc2d l WrapHorizontal (x - maxX, y)
  | otherwise   = (x, y)
wrapDisc2d l@(_, maxY) WrapVertical (x, y)
  | y < 0       = wrapDisc2d l WrapVertical (x, y + maxY)
  | y >= maxY   = wrapDisc2d l WrapVertical (x, y - maxY)
  | otherwise   = (x, y)
wrapDisc2d l WrapBoth c = 
                  wrapDisc2d l WrapHorizontal $ wrapDisc2d l WrapVertical  c

topLeftDelta :: Discrete2dCoord
topLeftDelta      = (-1, -1)
topDelta :: Discrete2dCoord
topDelta          = ( 0, -1)
topRightDelta :: Discrete2dCoord
topRightDelta     = ( 1, -1)
leftDelta :: Discrete2dCoord
leftDelta         = (-1,  0)
rightDelta :: Discrete2dCoord
rightDelta        = ( 1,  0)
bottomLeftDelta :: Discrete2dCoord
bottomLeftDelta   = (-1,  1)
bottomDelta :: Discrete2dCoord
bottomDelta       = ( 0,  1)
bottomRightDelta :: Discrete2dCoord
bottomRightDelta  = ( 1,  1)
-------------------------------------------------------------------------------