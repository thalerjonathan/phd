{-# LANGUAGE Arrows #-}
module FrABS.Env.Environment (
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
    moore,

     wrapCells
  ) where

import FRP.Yampa
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List
import Data.Array.IArray
import Control.Monad.Random
import Control.Monad.Trans.State

{-
an Environment is a container which contains Agents and allows them to move arround
    => assigns an agent a position within a space
    => allows an agent to query its neighbourhood within this space
    => allows an agent to change its location
    => has a signal-function which is invoked once per iteration to allow the environment to behave e.g. regrow ressources which were harvested by the agents
-}

-- TODO: continuous environment: instead of Int, use Double -> can we do this by params?
    -- or use type-classes?
-- TODO: can we generalize to higher dimensions?
    
--data (Num d) => EnvCoordGeneric d = EnvCoordGeneric (d, d)
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

neighbourEdges :: Environment c l -> Node -> [l]
neighbourEdges env node = map fst ls
    where
        ls = neighbourLinks env node 

neighbourNodes :: Environment c l -> Node -> [Node]
neighbourNodes env node = map snd ls
    where
        ls = neighbourLinks env node 

neighbourNodesM :: Node -> State (Environment c l) [Node]
neighbourNodesM node = state (\env -> (neighbourNodes env node, env))

neighbourLinks :: Environment c l -> Node -> Adj l
neighbourLinks env node = lneighbors gr node
    where
        gr = envGraph env

directLinkBetween :: Environment c l -> Node -> Node -> Maybe l
directLinkBetween env n1 n2 = 
    do
        let ls = neighbourLinks env n1
        (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
        return linkLabel

directLinkBetweenM :: Node -> Node -> State (Environment c l) (Maybe l)
directLinkBetweenM n1 n2 = state (\env -> (directLinkBetween env n1 n2, env))

allCellsWithCoords :: Environment c l -> [(EnvCoord, c)]
allCellsWithCoords env = assocs ec
    where
        ec = envCells env

updateEnvironmentCells :: Environment c l -> (c -> c) -> Environment c l
updateEnvironmentCells env mecf = env { envCells = ec' }
    where
        ec = envCells env
        ec' = amap mecf ec

updateEnvironmentCellsWithCoords :: Environment c l -> ((EnvCoord, c) -> c) -> Environment c l
updateEnvironmentCellsWithCoords env mecf = env'
    where
        ecs = allCellsWithCoords env
        cs = map mecf ecs
        ecCoords = map fst ecs
        env' = foldr (\(coord, c) accEnv -> changeCellAt accEnv coord c) env (zip ecCoords cs)

changeCellAt :: Environment c l -> EnvCoord -> c -> Environment c l
changeCellAt env coord c = env { envCells = arr' }
    where
        arr = envCells env
        arr' = arr // [(coord, c)]

changeCellAtM :: EnvCoord -> c -> State (Environment c l) ()
changeCellAtM coord c = state (\env -> ((), changeCellAt env coord c))

distanceManhattan :: EnvCoord -> EnvCoord -> Int
distanceManhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclidean :: EnvCoord -> EnvCoord -> Double
distanceEuclidean (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = fromRational $ toRational (x2 - x1)
        yDelta = fromRational $ toRational (y2 - y1)

cellsAroundRadius :: Environment c l -> EnvCoord -> Double -> [(EnvCoord, c)]
cellsAroundRadius env pos r = filter (\(coord, _) -> r >= (distanceEuclidean pos coord)) ecs 
    where
        ecs = allCellsWithCoords env
        -- TODO: does not yet wrap around boundaries

cellsAroundRadiusM :: EnvCoord -> Double -> State (Environment c l) [(EnvCoord, c)]
cellsAroundRadiusM pos r = state (\env -> (cellsAroundRadius env pos r, env))

cellsAroundRect :: Environment c l -> EnvCoord -> Int -> [(EnvCoord, c)]
cellsAroundRect env (cx, cy) r = zip wrappedCs cells
    where
        cs = [(x, y) | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
        l = (envLimits env)
        w = (envWrapping env)
        wrappedCs = wrapCells l w cs
        cells = cellsAt env wrappedCs

cellsAt :: Environment c l -> [EnvCoord] -> [c]
cellsAt env cs = map (arr !) cs
    where
        arr = envCells env

cellAt :: Environment c l -> EnvCoord -> c
cellAt env coord = arr ! coord
    where
        arr = envCells env

cellAtM :: EnvCoord -> State (Environment c l) c 
cellAtM coord = state (\env -> (cellAt env coord, env))
   

randomCell :: Environment c l -> Rand StdGen (c, EnvCoord)
randomCell env = 
    do
        let (maxX, maxY) = envLimits env

        randX <- getRandomR (0, maxX - 1) 
        randY <- getRandomR (0, maxY - 1)

        let randCoord = (randX, randY)
        let randCell = cellAt env randCoord

        return (randCell, randCoord)
        
randomCellWithinRect :: Environment c l
                        -> EnvCoord 
                        -> Int 
                        -> Rand StdGen (c, EnvCoord)
randomCellWithinRect env (x, y) r = 
    do
        randX <- getRandomR (-r, r)
        randY <- getRandomR (-r, r)
        
        let randCoord = (x + randX, y + randY)
        let randCoordWrapped = wrap (envLimits env) (envWrapping env) randCoord
        let randCell = cellAt env randCoordWrapped

        return (randCell, randCoordWrapped)

neighboursDistance :: Environment c l -> EnvCoord -> Int -> [(EnvCoord, c)]
neighboursDistance env coord dist = zip wrappedNs cells
    where
        n = envNeighbourhood env
        coordDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. dist]
        l = envLimits env
        w = envWrapping env
        ns = neighbourhoodOf coord coordDeltas
        wrappedNs = wrapNeighbourhood l w ns
        cells = cellsAt env wrappedNs

neighboursDistanceM :: EnvCoord -> Int -> State (Environment c l) [(EnvCoord, c)]
neighboursDistanceM coord dist = state (\env -> (neighboursDistance env coord dist, env))

neighbours :: Environment c l -> EnvCoord -> [(EnvCoord, c)]
neighbours env coord = neighboursDistance env coord 1

neighboursM :: EnvCoord -> State (Environment c l) [(EnvCoord, c)]
neighboursM coord = state (\env -> (neighbours env coord, env))

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

