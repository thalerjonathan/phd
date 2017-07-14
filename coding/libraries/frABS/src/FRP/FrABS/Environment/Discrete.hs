module FRP.FrABS.Environment.Discrete (
    EnvironmentBehaviour,
    EnvCoord,
    EnvLimits,
    EnvNeighbourhood,
    EnvWrapping (..),
    EnvGraph,

    Environment (..),

    createEnvironment,

    neighbourEdges,
    neighbourNodes,
    neighbourNodesM,
    neighbourLinks,
    directLinkBetween,
    directLinkBetweenM,

    allCellsWithCoords,

    updateEnvironmentCells,
    updateEnvironmentCellsWithCoords,
    changeCellAt,
    changeCellAtM,

    distanceManhattan,
    distanceEuclidean,

    cellsAroundRadius,
    cellsAroundRadiusM,
    cellsAroundRect,

    cellsAt,
    cellAt,
    cellAtM,

    randomCell,
    randomCellWithinRect,

    neighbours,
    neighboursM,
    neighboursDistance,
    neighboursDistanceM,
    neighbourhoodOf,
    neighbourhoodScale,
   
    neumann,
    neumannSelf,
    moore,
    mooreSelf,

    wrapCells
  ) where

import FRP.Yampa
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Data.Array.IArray
import Control.Monad.Random
import Control.Monad.Trans.State

type EnvironmentBehaviour c l = SF (Environment c l) (Environment c l)
type EnvCoord = (Int, Int)
type EnvLimits = (Int, Int)
type EnvNeighbourhood = [(Int, Int)]
data EnvWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

type EnvGraph l = Gr () l

data Environment c l = Environment {
    envBehaviour :: Maybe (EnvironmentBehaviour c l),
    envLimits :: EnvLimits,
    envNeighbourhood :: EnvNeighbourhood,
    envWrapping :: EnvWrapping,
    envCells :: Array EnvCoord c,
    envRng :: StdGen,
    envGraph :: EnvGraph l
}

createEnvironment :: Maybe (EnvironmentBehaviour c l) ->
                        EnvLimits ->
                        EnvNeighbourhood ->
                        EnvWrapping ->
                        [(EnvCoord, c)] ->
                        StdGen -> 
                        Maybe (EnvGraph l) ->
                        Environment c l
createEnvironment beh
                    l@(xLimit, yLimit)
                    n
                    w
                    cs 
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

neighbourEdges :: Node -> Environment c l ->  [l]
neighbourEdges node env = map fst ls
    where
        ls = neighbourLinks node env

neighbourNodes :: Node -> Environment c l -> [Node]
neighbourNodes node env = map snd ls
    where
        ls = neighbourLinks node env

neighbourNodesM :: Node -> State (Environment c l) [Node]
neighbourNodesM node = state (\env -> (neighbourNodes node env, env))

neighbourLinks :: Node -> Environment c l -> Adj l
neighbourLinks node env = lneighbors gr node
    where
        gr = envGraph env

directLinkBetween :: Node -> Node -> Environment c l -> Maybe l
directLinkBetween n1 n2 env = 
    do
        let ls = neighbourLinks n1 env
        (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
        return linkLabel

directLinkBetweenM :: Node -> Node -> State (Environment c l) (Maybe l)
directLinkBetweenM n1 n2 = state (\env -> (directLinkBetween n1 n2 env, env))

allCellsWithCoords :: Environment c l -> [(EnvCoord, c)]
allCellsWithCoords env = assocs ec
    where
        ec = envCells env

updateEnvironmentCells :: (c -> c) -> Environment c l -> Environment c l
updateEnvironmentCells f env = env { envCells = ec' }
    where
        ec = envCells env
        ec' = amap f ec

updateEnvironmentCellsWithCoords :: ((EnvCoord, c) -> c) -> Environment c l -> Environment c l
updateEnvironmentCellsWithCoords f env = env'
    where
        ecs = allCellsWithCoords env
        cs = map f ecs
        ecCoords = map fst ecs
        env' = foldr (\(coord, c) accEnv -> changeCellAt coord c accEnv) env (zip ecCoords cs)

changeCellAt :: EnvCoord -> c -> Environment c l -> Environment c l
changeCellAt coord c env = env { envCells = arr' }
    where
        arr = envCells env
        arr' = arr // [(coord, c)]

changeCellAtM :: EnvCoord -> c -> State (Environment c l) ()
changeCellAtM coord c = state (\env -> ((), changeCellAt coord c env))

distanceManhattan :: EnvCoord -> EnvCoord -> Int
distanceManhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclidean :: EnvCoord -> EnvCoord -> Double
distanceEuclidean (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = fromRational $ toRational (x2 - x1)
        yDelta = fromRational $ toRational (y2 - y1)

cellsAroundRadius :: EnvCoord -> Double -> Environment c l -> [(EnvCoord, c)]
cellsAroundRadius  pos r env = filter (\(coord, _) -> r >= (distanceEuclidean pos coord)) ecs 
    where
        ecs = allCellsWithCoords env
        -- TODO: does not yet wrap around boundaries

cellsAroundRadiusM :: EnvCoord -> Double -> State (Environment c l) [(EnvCoord, c)]
cellsAroundRadiusM pos r = state (\env -> (cellsAroundRadius pos r env, env))

cellsAroundRect :: EnvCoord -> Int -> Environment c l -> [(EnvCoord, c)]
cellsAroundRect (cx, cy) r env = zip wrappedCs cells
    where
        cs = [(x, y) | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
        l = (envLimits env)
        w = (envWrapping env)
        wrappedCs = wrapCells l w cs
        cells = cellsAt wrappedCs env

cellsAt :: [EnvCoord] -> Environment c l -> [c]
cellsAt cs env = map (arr !) cs
    where
        arr = envCells env

cellAt :: EnvCoord -> Environment c l -> c
cellAt coord env = arr ! coord
    where
        arr = envCells env

cellAtM :: EnvCoord -> State (Environment c l) c 
cellAtM coord = state (\env -> (cellAt coord env, env))
   

randomCell :: Environment c l -> Rand StdGen (c, EnvCoord)
randomCell env = 
    do
        let (maxX, maxY) = envLimits env

        randX <- getRandomR (0, maxX - 1) 
        randY <- getRandomR (0, maxY - 1)

        let randCoord = (randX, randY)
        let randCell = cellAt randCoord env

        return (randCell, randCoord)
        
randomCellWithinRect ::  EnvCoord 
                        -> Int 
                        -> Environment c l
                        -> Rand StdGen (c, EnvCoord)
randomCellWithinRect (x, y) r env = 
    do
        randX <- getRandomR (-r, r)
        randY <- getRandomR (-r, r)
        
        let randCoord = (x + randX, y + randY)
        let randCoordWrapped = wrap (envLimits env) (envWrapping env) randCoord
        let randCell = cellAt randCoordWrapped env

        return (randCell, randCoordWrapped)

neighboursDistance :: EnvCoord -> Int -> Environment c l -> [(EnvCoord, c)]
neighboursDistance  coord dist env = zip wrappedNs cells
    where
        n = envNeighbourhood env
        coordDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. dist]
        l = envLimits env
        w = envWrapping env
        ns = neighbourhoodOf coord coordDeltas
        wrappedNs = wrapNeighbourhood l w ns
        cells = cellsAt wrappedNs env

neighboursDistanceM :: EnvCoord -> Int -> State (Environment c l) [(EnvCoord, c)]
neighboursDistanceM coord dist = state (\env -> (neighboursDistance coord dist env, env))

neighbours :: EnvCoord -> Environment c l -> [(EnvCoord, c)]
neighbours coord env = neighboursDistance coord 1 env

neighboursM :: EnvCoord -> State (Environment c l) [(EnvCoord, c)]
neighboursM coord = state (\env -> (neighbours coord env, env))

neighbourhoodOf :: EnvCoord -> EnvNeighbourhood -> EnvNeighbourhood
neighbourhoodOf (x,y) ns = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: EnvNeighbourhood -> Int -> EnvNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

wrapCells :: EnvLimits -> EnvWrapping -> EnvNeighbourhood -> EnvNeighbourhood
wrapCells = wrapNeighbourhood

-- NOTE: neumann-neighbourhood only exists in discrete spatial environments
neumann :: EnvNeighbourhood
neumann = [topDelta, leftDelta, rightDelta, bottomDelta]
neumannSelf :: EnvNeighbourhood
neumannSelf = [topDelta, leftDelta, centerDelta, rightDelta, bottomDelta]

-- NOTE: moore-neighbourhood only exists in discrete spatial environments
moore :: EnvNeighbourhood
moore = [topLeftDelta, topDelta, topRightDelta,
         leftDelta, rightDelta,
         bottomLeftDelta, bottomDelta, bottomRightDelta]
mooreSelf :: EnvNeighbourhood
mooreSelf = [topLeftDelta, topDelta, topRightDelta,
             leftDelta, centerDelta, rightDelta,
             bottomLeftDelta, bottomDelta, bottomRightDelta]

------------------------------------------------------------------------------------------------------------------------
-- GENERAL SPATIAL
------------------------------------------------------------------------------------------------------------------------


wrapNeighbourhood :: EnvLimits -> EnvWrapping -> EnvNeighbourhood -> EnvNeighbourhood
wrapNeighbourhood l w ns = map (wrap l w) ns

------------------------------------------------------------------------------------------------------------------------
-- internal stuff
------------------------------------------------------------------------------------------------------------------------
-- TODO: should not be used to clip because clipping and wrapping are two different things: clip REMOVES vertices, wrap just changes them
wrap :: EnvLimits -> EnvWrapping -> EnvCoord -> EnvCoord
wrap (maxX, maxY) ClipToMax (x, y) = (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1)))
wrap l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrap l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrap l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrap l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrap l WrapVertical (x, y + maxY)
    | y >= maxY = wrap l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrap l WrapBoth c = wrap l WrapHorizontal $ wrap l WrapVertical  c


topLeftDelta =       (-1, -1)
topDelta =           ( 0, -1)
topRightDelta =      ( 1, -1)
leftDelta =          (-1,  0)
centerDelta =        ( 0,  0)
rightDelta =         ( 1,  0)
bottomLeftDelta =    (-1,  1)
bottomDelta =        ( 0,  1)
bottomRightDelta =   ( 1,  1)
------------------------------------------------------------------------------------------------------------------------

