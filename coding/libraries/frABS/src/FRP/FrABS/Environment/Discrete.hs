module FRP.FrABS.Environment.Discrete (
    Discrete2DEnvironmentData (..),

    createEnvironment,

    EnvironmentDiscrete2D (..),
    
    distanceManhattanDisc2D,
    distanceEuclideanDisc2D,
    neighbourhoodOf,
    neighbourhoodScale,
    wrapCells,
    neumann,
    neumannSelf,
    moore,
    mooreSelf,
    wrapNeighbourhood,
    wrapDisc2D
  ) where

import FRP.FrABS.Environment.Definitions

import FRP.Yampa

import Data.List
import Data.Array.IArray
import Control.Monad.Random
import Control.Monad.Trans.State

data Discrete2DEnvironmentData = Discrete2DEnvironmentData c {
    envDisc2dBehaviour :: Maybe (EnvironmentBehaviour (Discrete2DEnvironmentData c)),
    envDisc2dLimits :: Discrete2DLimit,
    envDisc2dNeighbourhood :: Discrete2DNeighbourhood,
    envDisc2dWrapping :: EnvironmentWrapping,
    envDisc2dCells :: Array Discrete2DCoord c,
    envDisc2dRng :: StdGen
}

createEnvironment :: Maybe (EnvironmentBehaviour c l) 
                        -> EnvLimits
                        -> EnvNeighbourhood
                        -> EnvWrapping
                        -> [(EnvCoord, c)]
                        -> StdGen
                        -> Maybe (EnvGraph l)
                        -> Environment c l
createEnvironment beh l@(xLimit, yLimit) n w cs 
                    rng 
                    mayGr = Environment {
                             envBehaviour = beh,
                             envLimits = l,
                             envNeighbourhood = n,
                             envWrapping = w,
                             envCells = arr,
                             envRng = rng,
                             envGraph = maybe empty id mayGr
                         }
    where
        arr = array ((0, 0), (xLimit - 1, yLimit - 1)) cs

instance EnvironmentDiscrete2D (Discrete2DEnvironmentData c) where
    environmentBehaviour = Nothing

    --agentCoordDisc2D :: AgentId -> e -> Discrete2DCoord
    agentCoordDisc2D aid _ = (0, 0) -- TODO: implement

    -- environmentLimits :: e -> Discrete2DLimit
    environmentLimits e = envLimits e

    -- allCellsWithCoords :: e -> [(Discrete2DCoord, c)]
    allCellsWithCoords e = assocs ec
        where
            ec = envCells e

    -- updateEnvironmentCells :: (c -> c) -> e-> e
    updateEnvironmentCells f e = e { envCells = ec' }
        where
            ec = envCells e
            ec' = amap f ec

    --updateEnvironmentCellsWithCoords :: ((Discrete2DCoord, c) -> c) -> e -> e
    updateEnvironmentCellsWithCoords f e = e'
        where
            ecs = allCellsWithCoords e
            cs = map f ecs
            ecCoords = map fst ecs
            e' = foldr (\(coord, c) accEnv -> changeCellAt coord c accEnv) e (zip ecCoords cs)

    -- changeCellAt :: Discrete2DCoord -> c -> e -> e
    changeCellAt coord c e = e { envCells = arr' }
        where
            arr = envCells e
            arr' = arr // [(coord, c)]

    -- changeCellAtM :: Discrete2DCoord -> c -> State e ()
    changeCellAtM coord c = state (\e -> ((), changeCellAt coord c e))

    -- cellsAroundRadius :: Discrete2DCoord -> Double -> e -> [(Discrete2DCoord, c)]
    cellsAroundRadius  pos r e = filter (\(coord, _) -> r >= (distanceEuclidean pos coord)) ecs 
        where
            ecs = allCellsWithCoords e
            -- TODO: does not yet wrap around boundaries

    -- cellsAroundRadiusM :: Discrete2DCoord -> Double -> State e [(Discrete2DCoord, c)]
    cellsAroundRadiusM pos r = state (\e -> (cellsAroundRadius pos r e, e))

    -- cellsAroundRect :: Discrete2DCoord -> Int -> e -> [(Discrete2DCoord, c)]
    cellsAroundRect (cx, cy) r e = zip wrappedCs cells
        where
            cs = [(x, y) | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
            l = (envLimits e)
            w = (envWrapping e)
            wrappedCs = wrapCells l w cs
            cells = cellsAt wrappedCs e

    -- cellsAt :: [Discrete2DCoord] -> e -> [c]
    cellsAt cs e = map (arr !) cs
        where
            arr = envCells e

    -- cellAt :: Discrete2DCoord -> e -> c
    cellAt coord e = arr ! coord
        where
            arr = envCells e

    -- cellAtM :: Discrete2DCoord -> State e c
    cellAtM coord = state (\e -> (cellAt coord e, e))

    -- randomCell :: e -> Rand StdGen (c, Discrete2DCoord)
    randomCell e = 
        do
            let (maxX, maxY) = envLimits e

            randX <- getRandomR (0, maxX - 1) 
            randY <- getRandomR (0, maxY - 1)

            let randCoord = (randX, randY)
            let randCell = cellAt randCoord e

            return (randCell, randCoord)

    -- randomCellWithinRect :: Discrete2DCoord -> Int -> e -> Rand StdGen (c, Discrete2DCoord)
    randomCellWithinRect (x, y) r e = 
        do
            randX <- getRandomR (-r, r)
            randY <- getRandomR (-r, r)
            
            let randCoord = (x + randX, y + randY)
            let randCoordWrapped = wrap (envLimits e) (envWrapping e) randCoord
            let randCell = cellAt randCoordWrapped e

            return (randCell, randCoordWrapped)

    -- neighboursDistance :: Discrete2DCoord -> Int -> e -> [(Discrete2DCoord, c)]
    neighboursDistance  coord dist e = zip wrappedNs cells
        where
            n = envNeighbourhood e
            coordDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. dist]
            l = envLimits e
            w = envWrapping e
            ns = neighbourhoodOf coord coordDeltas
            wrappedNs = wrapNeighbourhood l w ns
            cells = cellsAt wrappedNs e

    -- neighboursDistanceM :: Discrete2DCoord -> Int -> State e [(Discrete2DCoord, c)]
    neighboursDistanceM coord dist = state (\e -> (neighboursDistance coord dist e, e))

    -- neighbours :: Discrete2DCoord -> e -> [(Discrete2DCoord, c)]
    neighbours coord e = neighboursDistance coord 1 e

    neighboursM :: Discrete2DCoord -> State e [(Discrete2DCoord, c)]
    neighboursM coord = state (\e -> (neighbours coord e, e))
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
distanceManhattanDisc2D :: Discrete2DCoord -> Discrete2DCoord -> Int
distanceManhattanDisc2D (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanDisc2D :: Discrete2DCoord -> Discrete2DCoord -> Double
distanceEuclideanDisc2D (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = fromRational $ toRational (x2 - x1)
        yDelta = fromRational $ toRational (y2 - y1)

neighbourhoodOf :: Discrete2DCoord -> Discrete2DNeighbourhood -> Discrete2DNeighbourhood
neighbourhoodOf (x,y) ns = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: Discrete2DNeighbourhood -> Int -> Discrete2DNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: Discrete2DLimit -> EnvironmentWrapping -> Discrete2DNeighbourhood -> Discrete2DNeighbourhood
wrapCells = wrapNeighbourhood

neumann :: Discrete2DNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]

neumannSelf :: Discrete2DNeighbourhood
neumannSelf = [topDelta, leftDelta, centerDelta, rightDelta, bottomDelta]

moore :: Discrete2DNeighbourhood
moore = [topLeftDelta, topDelta, topRightDelta,
         leftDelta, rightDelta,
         bottomLeftDelta, bottomDelta, bottomRightDelta]

mooreSelf :: Discrete2DNeighbourhood
mooreSelf = [topLeftDelta, topDelta, topRightDelta,
             leftDelta, centerDelta, rightDelta,
             bottomLeftDelta, bottomDelta, bottomRightDelta]

wrapNeighbourhood :: Discrete2DLimit -> EnvironmentWrapping -> Discrete2DNeighbourhood -> Discrete2DNeighbourhood
wrapNeighbourhood l w ns = map (wrap l w) ns

wrapDisc2D :: Discrete2DLimit -> EnvironmentWrapping -> Discrete2DCoord -> Discrete2DCoord
wrapDisc2D (maxX, maxY) ClipToMax (x, y) = (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1)))
wrapDisc2D l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrap l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrap l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrapDisc2D l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrap l WrapVertical (x, y + maxY)
    | y >= maxY = wrap l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrapDisc2D l WrapBoth c = wrap l WrapHorizontal $ wrap l WrapVertical  c

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