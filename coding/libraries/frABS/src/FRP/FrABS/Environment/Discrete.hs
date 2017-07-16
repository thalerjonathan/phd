module FRP.FrABS.Environment.Discrete (
    Discrete2dDimension,
    Discrete2dCoord,
    Discrete2dNeighbourhood,

    Discrete2d (..), -- TODO: hide data-constructor
    Discrete2dNetwork (..),
    
    createDiscrete2d,
 
    agentCoordDisc2d,
    updateAgentCoordDisc2d,
    allCellsWithCoords,
    updateEnvironmentCells,
    updateEnvironmentCellsWithCoords,
    changeCellAt,
    changeCellAtM,
    cellsAroundRadius,
    cellsAroundRadiusM,
    cellsAroundRect,
    cellsAt,
    cellAt,
    cellAtM,
    randomCell,
    randomCellWithinRect,
    neighboursDistance,
    neighboursDistanceM,
    neighbours,
    neighboursM,

    distanceManhattanDisc2d,
    distanceEuclideanDisc2d,
    neighbourhoodOf,
    neighbourhoodScale,
    wrapCells,
    neumann,
    neumannSelf,
    moore,
    mooreSelf,
    wrapNeighbourhood,
    wrapDisc2d
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Definitions
import FRP.FrABS.Environment.Network

import FRP.Yampa

import Data.List
import Data.Array.IArray
import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Data.Map as Map

type Discrete2dDimension = (Int, Int)
type Discrete2dCoord = Discrete2dDimension

type Discrete2dNeighbourhood = [Discrete2dCoord]

data Discrete2d c = Discrete2d {
    envDisc2dBehaviour :: Maybe (EnvironmentBehaviour (Discrete2d c)),
    envDisc2dDims :: Discrete2dDimension,
    envDisc2dNeighbourhood :: Discrete2dNeighbourhood,
    envDisc2dWrapping :: EnvironmentWrapping,
    envDisc2dCells :: Array Discrete2dCoord c,
    envDisc2dRng :: StdGen,
    envDisc2dAgentPositions :: Map.Map AgentId Discrete2dCoord
}

data Discrete2dNetwork l c = Discrete2dNetwork {
    envCombDisc2dNetwork :: Network l,
    envCombDisc2d :: Discrete2d  c
}

createDiscrete2d :: Maybe (EnvironmentBehaviour (Discrete2d c)) 
                        -> Discrete2dDimension
                        -> Discrete2dNeighbourhood
                        -> EnvironmentWrapping
                        -> [(Discrete2dCoord, c)]
                        -> StdGen
                        -> Discrete2d c
createDiscrete2d beh d@(xLimit, yLimit) n w cs rng 
                        = Discrete2d {
                            envDisc2dBehaviour = beh,
                            envDisc2dDims = d,
                            envDisc2dNeighbourhood = n,
                            envDisc2dWrapping = w,
                            envDisc2dCells = arr,
                            envDisc2dRng = rng,
                            envDisc2dAgentPositions = Map.empty
                        }
    where
        arr = array ((0, 0), (xLimit - 1, yLimit - 1)) cs

agentCoordDisc2d :: AgentId -> Discrete2d c -> Discrete2dCoord
agentCoordDisc2d aid _ = (0, 0) -- TODO: implement

updateAgentCoordDisc2d :: AgentId -> Discrete2dCoord -> Discrete2d c -> Discrete2d c
updateAgentCoordDisc2d aid coord e = e -- TODO: implement


allCellsWithCoords :: Discrete2d c -> [(Discrete2dCoord, c)]
allCellsWithCoords e = assocs $ envDisc2dCells e

updateEnvironmentCells :: (c -> c) -> Discrete2d c -> Discrete2d c
updateEnvironmentCells f e = e { envDisc2dCells = ec' }
    where
        ec = envDisc2dCells e
        ec' = amap f ec

updateEnvironmentCellsWithCoords :: ((Discrete2dCoord, c) -> c) -> Discrete2d c -> Discrete2d c
updateEnvironmentCellsWithCoords f e = e'
    where
        ecs = allCellsWithCoords e
        cs = map f ecs
        ecCoords = map fst ecs
        e' = foldr (\(coord, c) accEnv -> changeCellAt coord c accEnv) e (zip ecCoords cs)

changeCellAt :: Discrete2dCoord -> c -> Discrete2d c -> Discrete2d c
changeCellAt coord c e = e { envDisc2dCells = arr' }
    where
        arr = envDisc2dCells e
        arr' = arr // [(coord, c)]

changeCellAtM :: Discrete2dCoord -> c -> State (Discrete2d c) ()
changeCellAtM coord c = state (\e -> ((), changeCellAt coord c e))

cellsAroundRadius :: Discrete2dCoord -> Double -> Discrete2d c -> [(Discrete2dCoord, c)]
cellsAroundRadius  pos r e = filter (\(coord, _) -> r >= (distanceEuclideanDisc2d pos coord)) ecs 
    where
        ecs = allCellsWithCoords e
        -- TODO: does not yet wrap around boundaries

cellsAroundRadiusM :: Discrete2dCoord -> Double -> State (Discrete2d c) [(Discrete2dCoord, c)]
cellsAroundRadiusM pos r = state (\e -> (cellsAroundRadius pos r e, e))

cellsAroundRect :: Discrete2dCoord -> Int -> Discrete2d c -> [(Discrete2dCoord, c)]
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

randomCell :: Discrete2d c -> Rand StdGen (c, Discrete2dCoord)
randomCell e = 
    do
        let (maxX, maxY) = envDisc2dDims e

        randX <- getRandomR (0, maxX - 1) 
        randY <- getRandomR (0, maxY - 1)

        let randCoord = (randX, randY)
        let randCell = cellAt randCoord e

        return (randCell, randCoord)

randomCellWithinRect :: Discrete2dCoord -> Int -> Discrete2d c -> Rand StdGen (c, Discrete2dCoord)
randomCellWithinRect (x, y) r e = 
    do
        randX <- getRandomR (-r, r)
        randY <- getRandomR (-r, r)
        
        let randCoord = (x + randX, y + randY)
        let randCoordWrapped = wrapDisc2d (envDisc2dDims e) (envDisc2dWrapping e) randCoord
        let randCell = cellAt randCoordWrapped e

        return (randCell, randCoordWrapped)

neighboursDistance :: Discrete2dCoord -> Int -> Discrete2d c -> [(Discrete2dCoord, c)]
neighboursDistance  coord dist e = zip wrappedNs cells
    where
        n = envDisc2dNeighbourhood e
        coordDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. dist]
        l = envDisc2dDims e
        w = envDisc2dWrapping e
        ns = neighbourhoodOf coord coordDeltas
        wrappedNs = wrapNeighbourhood l w ns
        cells = cellsAt wrappedNs e

neighboursDistanceM :: Discrete2dCoord -> Int -> State (Discrete2d c) [(Discrete2dCoord, c)]
neighboursDistanceM coord dist = state (\e -> (neighboursDistance coord dist e, e))

neighbours :: Discrete2dCoord -> (Discrete2d c) -> [(Discrete2dCoord, c)]
neighbours coord e = neighboursDistance coord 1 e

neighboursM :: Discrete2dCoord -> State (Discrete2d c) [(Discrete2dCoord, c)]
neighboursM coord = state (\e -> (neighbours coord e, e))
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
distanceManhattanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Int
distanceManhattanDisc2d (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanDisc2d :: Discrete2dCoord -> Discrete2dCoord -> Double
distanceEuclideanDisc2d (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = fromRational $ toRational (x2 - x1)
        yDelta = fromRational $ toRational (y2 - y1)

neighbourhoodOf :: Discrete2dCoord -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
neighbourhoodOf (x,y) ns = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: Discrete2dNeighbourhood -> Int -> Discrete2dNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
wrapCells = wrapNeighbourhood

neumann :: Discrete2dNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]

neumannSelf :: Discrete2dNeighbourhood
neumannSelf = [topDelta, leftDelta, centerDelta, rightDelta, bottomDelta]

moore :: Discrete2dNeighbourhood
moore = [topLeftDelta, topDelta, topRightDelta,
         leftDelta, rightDelta,
         bottomLeftDelta, bottomDelta, bottomRightDelta]

mooreSelf :: Discrete2dNeighbourhood
mooreSelf = [topLeftDelta, topDelta, topRightDelta,
             leftDelta, centerDelta, rightDelta,
             bottomLeftDelta, bottomDelta, bottomRightDelta]

wrapNeighbourhood :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dNeighbourhood -> Discrete2dNeighbourhood
wrapNeighbourhood l w ns = map (wrapDisc2d l w) ns

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

topLeftDelta =       (-1, -1)
topDelta =           ( 0, -1)
topRightDelta =      ( 1, -1)
leftDelta =          (-1,  0)
centerDelta =        ( 0,  0)
rightDelta =         ( 1,  0)
bottomLeftDelta =    (-1,  1)
bottomDelta =        ( 0,  1)
bottomRightDelta =   ( 1,  1)
-------------------------------------------------------------------------------