module FRP.Chimera.Environment.Discrete 
  (
    Discrete2dDimension
  , Discrete2dCoord
  , Discrete2dNeighbourhood
  , Discrete2dCell

  , Discrete2d (..)

  , SingleOccupantCell
  , SingleOccupantDiscrete2d
  , MultiOccupantCell
  , MultiOccupantDiscrete2d

  , createDiscrete2d

  , dimensionsDisc2d
  , dimensionsDisc2dM

  , allCellsWithCoords
  , updateCells
  , updateCellsM
  , updateCellsWithCoords
  , updateCellsWithCoordsM
  , updateCellAt
  , changeCellAt
  , changeCellAtM
  , cellsAroundRadius
  , cellsAroundRadiusM
  , cellsAroundRect
  , cellsAt
  , cellAt
  , cellAtM
  , randomCell
  , randomCellWithinRect
  --, environmentDisc2dRandom

  , neighbours
  , neighboursM
  , neighbourCells
  , neighbourCellsM

  , neighboursInNeumannDistance
  , neighboursInNeumannDistanceM
  , neighboursCellsInNeumannDistance
  , neighboursCellsInNeumannDistanceM

  , distanceManhattanDisc2d
  , distanceEuclideanDisc2d
  , neighbourhoodOf
  , neighbourhoodScale
  , wrapCells
  , neumann
  , moore
  , wrapNeighbourhood
  , wrapDisc2d
  , wrapDisc2dEnv

  , randomNeighbourCell
  , randomNeighbour

  , occupied
  , unoccupy
  , occupy
  , occupier
  , addOccupant
  , removeOccupant
  , hasOccupiers
  , occupiers
  ) where
    
import Data.Array.IArray
import Data.List
import Data.Maybe
import Control.Monad.Random
import Control.Monad.Trans.State

import FRP.Chimera.Environment.Spatial

type Discrete2dDimension      = (Int, Int)
type Discrete2dCoord          = Discrete2dDimension
type Discrete2dNeighbourhood  = [Discrete2dCoord]
type Discrete2dCell c         = (Discrete2dCoord, c)

type SingleOccupantCell c       = Maybe c
type SingleOccupantDiscrete2d c = Discrete2d (SingleOccupantCell c)

type MultiOccupantCell c        = [c]
type MultiOccupantDiscrete2d c  = Discrete2d (MultiOccupantCell c)

data Discrete2d c = Discrete2d 
  {
      envDisc2dDims           :: Discrete2dDimension
    , envDisc2dNeighbourhood  :: Discrete2dNeighbourhood
    , envDisc2dWrapping       :: EnvironmentWrapping
    , envDisc2dCells          :: Array Discrete2dCoord c
    -- , envDisc2dRng            :: StdGen
  } deriving (Show, Read)

createDiscrete2d :: Discrete2dDimension
                    -> Discrete2dNeighbourhood
                    -> EnvironmentWrapping
                    -> [Discrete2dCell c]
                    -- -> StdGen
                    -> Discrete2d c
createDiscrete2d d@(xLimit, yLimit) n w cs =
    Discrete2d {
      envDisc2dDims = d
    , envDisc2dNeighbourhood = n
    , envDisc2dWrapping = w
    , envDisc2dCells = arr
    --, envDisc2dRng = rng
    }
  where
    arr = array ((0, 0), (xLimit - 1, yLimit - 1)) cs

      {-
environmentDisc2dRandom :: Rand StdGen (Discrete2d c) -> Discrete2d c -> Discrete2d c
environmentDisc2dRandom f e = e''
    where
        g = envDisc2dRng e
        (e', g') = runRand f g
        e'' = e' { envDisc2dRng = g' }
-}

dimensionsDisc2d :: Discrete2d c -> Discrete2dDimension
dimensionsDisc2d = envDisc2dDims

dimensionsDisc2dM :: State (Discrete2d c) Discrete2dDimension
dimensionsDisc2dM = state (\e -> (envDisc2dDims e, e))

allCellsWithCoords :: Discrete2d c -> [Discrete2dCell c]
allCellsWithCoords e = assocs $ envDisc2dCells e

updateCellsM :: (c -> c) -> State (Discrete2d c) ()
updateCellsM f = state (\e -> ((), updateCells f e))

updateCells :: (c -> c) -> Discrete2d c -> Discrete2d c
updateCells f e = e { envDisc2dCells = ec' }
  where
    ec = envDisc2dCells e
    ec' = amap f ec

updateCellsWithCoordsM :: (Discrete2dCell c -> c) -> State (Discrete2d c) ()
updateCellsWithCoordsM f = state (\e -> ((), updateCellsWithCoords f e))

updateCellsWithCoords :: (Discrete2dCell c -> c) -> Discrete2d c -> Discrete2d c
updateCellsWithCoords f e = e'
  where
    ecs = allCellsWithCoords e
    cs = map f ecs
    ecCoords = map fst ecs
    e' = foldr (\(coord, c) accEnv -> changeCellAt coord c accEnv) e (zip ecCoords cs)

updateCellAt :: Discrete2dCoord -> (c -> c) -> Discrete2d c -> Discrete2d c
updateCellAt coord f e = e { envDisc2dCells = arr' }
  where
    arr = envDisc2dCells e
    c = arr ! coord
    c' = f c
    arr' = arr // [(coord, c')]

changeCellAt :: Discrete2dCoord -> c -> Discrete2d c -> Discrete2d c
changeCellAt coord c e = e { envDisc2dCells = arr' }
  where
    arr = envDisc2dCells e
    arr' = arr // [(coord, c)]

changeCellAtM :: Discrete2dCoord -> c -> State (Discrete2d c) ()
changeCellAtM coord c = state (\e -> ((), changeCellAt coord c e))

cellsAroundRadius :: Discrete2dCoord -> Double -> Discrete2d c -> [Discrete2dCell c]
cellsAroundRadius  pos r e = filter (\(coord, _) -> r >= distanceEuclideanDisc2d pos coord) ecs
  where
    ecs = allCellsWithCoords e
    -- TODO: does not yet wrap around boundaries

cellsAroundRadiusM :: Discrete2dCoord -> Double -> State (Discrete2d c) [Discrete2dCell c]
cellsAroundRadiusM pos r = state (\e -> (cellsAroundRadius pos r e, e))

cellsAroundRect :: Discrete2dCoord -> Int -> Discrete2d c -> [Discrete2dCell c]
cellsAroundRect (cx, cy) r e = zip wrappedCs cells
  where
    cs = [(x, y) | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
    l = envDisc2dDims e
    w = envDisc2dWrapping e
    wrappedCs = wrapCells l w cs
    cells = cellsAt wrappedCs e

cellsAt :: [Discrete2dCoord] -> Discrete2d c -> [c]
cellsAt cs e = map (arr !) cs
  where
    arr = envDisc2dCells e

cellAt :: Discrete2dCoord -> Discrete2d c -> c
cellAt coord e = arr ! coord
  where
    arr = envDisc2dCells e

cellAtM :: Discrete2dCoord -> State (Discrete2d c) c
cellAtM coord = state (\e -> (cellAt coord e, e))

randomCell :: RandomGen g => Discrete2d c -> Rand g (c, Discrete2dCoord)
randomCell e = do
  let (maxX, maxY) = envDisc2dDims e

  randX <- getRandomR (0, maxX - 1) 
  randY <- getRandomR (0, maxY - 1)

  let randCoord = (randX, randY)
  let randCell = cellAt randCoord e

  return (randCell, randCoord)

randomCellWithinRect :: RandomGen g =>
                        Discrete2dCoord 
                        -> Int 
                        -> Discrete2d c
                        -> Rand g (c, Discrete2dCoord)
randomCellWithinRect (x, y) r e =  do
  randX <- getRandomR (-r, r)
  randY <- getRandomR (-r, r)
  
  let randCoord = (x + randX, y + randY)
  let randCoordWrapped = wrapDisc2d (envDisc2dDims e) (envDisc2dWrapping e) randCoord
  let randCell = cellAt randCoordWrapped e

  return (randCell, randCoordWrapped)

-- NOTE: this function does only work for neumann-neighbourhood, it ignores the environments neighbourhood. also it does not include the coord itself
neighboursInNeumannDistance :: Discrete2dCoord -> Int -> Bool -> Discrete2d c -> [Discrete2dCell c]
neighboursInNeumannDistance coord dist ic e = zip wrappedNs cells
  where
    n = neumann
    coordDeltas = foldr (\v acc -> acc ++ neighbourhoodScale n v) [] [1 .. dist]
    l = envDisc2dDims e
    w = envDisc2dWrapping e
    ns = neighbourhoodOf coord ic coordDeltas
    wrappedNs = wrapNeighbourhood l w ns
    cells = cellsAt wrappedNs e

neighboursInNeumannDistanceM :: Discrete2dCoord -> Int -> Bool -> State (Discrete2d c) [Discrete2dCell c]
neighboursInNeumannDistanceM coord dist ic = state (\e -> (neighboursInNeumannDistance coord dist ic e, e))

neighboursCellsInNeumannDistance :: Discrete2dCoord -> Int -> Bool -> Discrete2d c -> [c]
neighboursCellsInNeumannDistance coord dist ic e = map snd (neighboursInNeumannDistance coord dist ic e)

neighboursCellsInNeumannDistanceM :: Discrete2dCoord -> Int -> Bool -> State (Discrete2d c) [c]
neighboursCellsInNeumannDistanceM coord dist ic = state (\e -> (neighboursCellsInNeumannDistance coord dist ic e, e))

neighbours :: Discrete2dCoord -> Bool -> Discrete2d c -> [Discrete2dCell c]
neighbours coord ic e = zip wrappedNs cells
  where
    n = envDisc2dNeighbourhood e
    l = envDisc2dDims e
    w = envDisc2dWrapping e
    ns = neighbourhoodOf coord ic n
    wrappedNs = wrapNeighbourhood l w ns
    cells = cellsAt wrappedNs e

neighboursM :: Discrete2dCoord -> Bool -> State (Discrete2d c) [Discrete2dCell c]
neighboursM coord ic = state (\e -> (neighbours coord ic e, e))

neighbourCells :: Discrete2dCoord -> Bool -> Discrete2d c -> [c]
neighbourCells coord ic e = map snd (neighbours coord ic e)

neighbourCellsM :: Discrete2dCoord -> Bool -> State (Discrete2d c) [c]
neighbourCellsM coord ic = state (\e -> (neighbourCells coord ic e, e))
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
distanceManhattanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Int
distanceManhattanDisc2d (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distanceEuclideanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Double
distanceEuclideanDisc2d (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
  where
    xDelta = fromRational $ toRational (x2 - x1)
    yDelta = fromRational $ toRational (y2 - y1)

neighbourhoodOf :: Discrete2dCoord -> Bool -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
neighbourhoodOf coord@(x,y) includeCoord ns 
    | includeCoord = coord : ns'
    | otherwise = ns'
  where
    ns' = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: Discrete2dNeighbourhood -> Int -> Discrete2dNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
wrapCells = wrapNeighbourhood

neumann :: Discrete2dNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]

moore :: Discrete2dNeighbourhood
moore = [ topLeftDelta,    topDelta,     topRightDelta,
          leftDelta,                     rightDelta,
          bottomLeftDelta, bottomDelta,  bottomRightDelta ]

wrapNeighbourhood :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
wrapNeighbourhood l w ns = map (wrapDisc2d l w) ns

wrapDisc2dEnv :: Discrete2d c -> Discrete2dCoord -> Discrete2dCoord
wrapDisc2dEnv e c = wrapDisc2d d w c
  where
    d = envDisc2dDims e
    w = envDisc2dWrapping e

wrapDisc2d :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dCoord -> Discrete2dCoord
wrapDisc2d (maxX, maxY) ClipToMax (x, y) = (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1)))
wrapDisc2d l@(maxX, _) WrapHorizontal (x, y)
  | x < 0 = wrapDisc2d l WrapHorizontal (x + maxX, y)
  | x >= maxX = wrapDisc2d l WrapHorizontal (x - maxX, y)
  | otherwise = (x, y)
wrapDisc2d l@(_, maxY) WrapVertical (x, y)
  | y < 0 = wrapDisc2d l WrapVertical (x, y + maxY)
  | y >= maxY = wrapDisc2d l WrapVertical (x, y - maxY)
  | otherwise = (x, y)
wrapDisc2d l WrapBoth c = wrapDisc2d l WrapHorizontal $ wrapDisc2d l WrapVertical  c

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

-------------------------------------------------------------------------------
-- UTILITIES
-------------------------------------------------------------------------------
randomNeighbourCell :: RandomGen g => Discrete2dCoord -> Bool -> Discrete2d c -> Rand g c
randomNeighbourCell pos ic e = randomNeighbour pos ic e >>= (\(_, c) -> return c)

randomNeighbour :: RandomGen g => Discrete2dCoord -> Bool -> Discrete2d c -> Rand g (Discrete2dCell c)
randomNeighbour pos ic e = do
  let ncc = neighbours pos ic e
  let l = length ncc 
  randIdx <- getRandomR (0, l - 1)
  return (ncc !! randIdx)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- OCCUPIERS
-------------------------------------------------------------------------------
occupied :: Discrete2dCoord -> SingleOccupantDiscrete2d c -> Bool
occupied coord e = isJust $ cellAt coord e

unoccupy :: Discrete2dCoord -> SingleOccupantDiscrete2d c -> SingleOccupantDiscrete2d c
unoccupy coord e = changeCellAt coord Nothing e

occupy :: Discrete2dCoord -> c -> SingleOccupantDiscrete2d c -> SingleOccupantDiscrete2d c
occupy coord c e = changeCellAt coord (Just c) e

occupier :: Discrete2dCoord -> SingleOccupantDiscrete2d c -> c
occupier coord e = fromJust $ cellAt coord e

addOccupant :: Discrete2dCoord -> c -> MultiOccupantDiscrete2d c -> MultiOccupantDiscrete2d c
addOccupant coord c e = updateCellAt coord (\cs -> c : cs) e

removeOccupant :: (Eq c) => Discrete2dCoord -> c -> MultiOccupantDiscrete2d c -> MultiOccupantDiscrete2d c
removeOccupant coord c e = updateCellAt coord (\cs -> delete c cs) e

hasOccupiers :: Discrete2dCoord -> MultiOccupantDiscrete2d c -> Bool
hasOccupiers coord e = not . null $ cellAt coord e

occupiers :: Discrete2dCoord -> MultiOccupantDiscrete2d c -> [c]
occupiers = cellAt
-------------------------------------------------------------------------------