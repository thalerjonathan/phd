{-# LANGUAGE Arrows #-}
module FrABS.Env.Environment where

import FRP.Yampa

import Data.Array.IArray
import Control.Monad.Random

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
-- TODO: graph environment: no geometrical space => no position but only neighbours
    -- TODO: https://hackage.haskell.org/package/containers-0.5.10.2/docs/Data-Graph.html
    -- TODO: https://hackage.haskell.org/package/fgl
    -- TODO: http://mazzo.li/posts/graph-drawing.html
    
--data (Num d) => EnvCoordGeneric d = EnvCoordGeneric (d, d)
type EnvironmentBehaviour c = SF (Environment c) (Environment c)
type EnvCoord = (Int, Int)
type EnvLimits = (Int, Int)
type EnvNeighbourhood = [(Int, Int)]
data EnvWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

data Environment c = Environment {
    envBehaviour :: Maybe (EnvironmentBehaviour c),
    envLimits :: EnvLimits,
    envNeighbourhood :: EnvNeighbourhood,
    envWrapping :: EnvWrapping,
    envCells :: Array EnvCoord c,
    envRng :: StdGen
}

createEnvironment :: Maybe (EnvironmentBehaviour c) ->
                        EnvLimits ->
                        EnvNeighbourhood ->
                        EnvWrapping ->
                        [(EnvCoord, c)] ->
                        StdGen -> 
                        Environment c
createEnvironment beh
                    l@(xLimit, yLimit)
                    n
                    w
                    cs 
                    rng = Environment {
                             envBehaviour = beh,
                             envLimits = l,
                             envNeighbourhood = n,
                             envWrapping = w,
                             envCells = arr,
                             envRng = rng
                         }
    where
        arr = array ((0, 0), (xLimit - 1, yLimit - 1)) cs

allCellsWithCoords :: Environment c -> [(EnvCoord, c)]
allCellsWithCoords env = assocs ec
    where
        ec = envCells env

updateEnvironmentCells :: Environment c -> (c -> c) -> Environment c
updateEnvironmentCells env mecf = env { envCells = ec' }
    where
        ec = envCells env
        ec' = amap mecf ec

updateEnvironmentCellsWithCoords :: Environment c -> ((EnvCoord, c) -> c) -> Environment c
updateEnvironmentCellsWithCoords env mecf = env'
    where
        ecs = allCellsWithCoords env
        cs = map mecf ecs
        ecCoords = map fst ecs
        env' = foldr (\(coord, c) accEnv -> changeCellAt accEnv coord c) env (zip ecCoords cs)

changeCellAt :: Environment c -> EnvCoord -> c -> Environment c
changeCellAt env coord c = env { envCells = arr' }
    where
        arr = envCells env
        arr' = arr // [(coord, c)]

distance :: EnvCoord -> EnvCoord -> Int
distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEucl :: EnvCoord -> EnvCoord -> Double
distanceEucl (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = fromRational $ toRational (x2 - x1)
        yDelta = fromRational $ toRational (y2 - y1)

cellsAroundRadius :: Environment c -> EnvCoord -> Double -> [(EnvCoord, c)]
cellsAroundRadius env pos r = filter (\(coord, _) -> r >= (distanceEucl pos coord)) ecs 
    where
        ecs = allCellsWithCoords env
        -- TODO: does not yet wrap around boundaries

cellsAround :: Environment c -> EnvCoord -> Int -> [(EnvCoord, c)]
cellsAround env (cx, cy) r = zip wrappedCs cells
    where
        cs = [(x, y) | x <- [cx - r .. cx + r], y <- [cy - r .. cy + r]]
        l = (envLimits env)
        w = (envWrapping env)
        wrappedCs = wrapCells l w cs
        cells = cellsAt env wrappedCs

cellsAt :: Environment c -> [EnvCoord] -> [c]
cellsAt env cs = map (arr !) cs
    where
        arr = envCells env

cellAt :: Environment c -> EnvCoord -> c
cellAt env coord = arr ! coord
    where
        arr = envCells env

randomCell :: Environment c -> Rand StdGen (c, EnvCoord)
randomCell env = 
    do
        let (maxX, maxY) = envLimits env

        randX <- getRandomR (0, maxX - 1) 
        randY <- getRandomR (0, maxY - 1)

        let randCoord = (randX, randY)
        let randCell = cellAt env randCoord

        return (randCell, randCoord)

randomCellWithRadius :: Environment c 
                        -> EnvCoord 
                        -> Int 
                        -> Rand StdGen (c, EnvCoord)
randomCellWithRadius env (x, y) r = 
    do
        randX <- getRandomR (-r, r)
        randY <- getRandomR (-r, r)
        
        let randCoord = (x + randX, y + randY)
        let randCoordWrapped = wrap (envLimits env) (envWrapping env) randCoord
        let randCell = cellAt env randCoordWrapped

        return (randCell, randCoordWrapped)

neighbours :: Environment c -> EnvCoord -> [(EnvCoord, c)]
neighbours env coord = zip wrappedNs cells
    where
        n = (envNeighbourhood env)
        l = (envLimits env)
        w = (envWrapping env)
        ns = neighbourhoodOf coord n
        wrappedNs = wrapNeighbourhood l w ns
        cells = cellsAt env wrappedNs

------------------------------------------------------------------------------------------------------------------------
-- GENERAL SPATIAL
------------------------------------------------------------------------------------------------------------------------
wrapCells :: EnvLimits -> EnvWrapping -> EnvNeighbourhood -> EnvNeighbourhood
wrapCells = wrapNeighbourhood

wrapNeighbourhood :: EnvLimits -> EnvWrapping -> EnvNeighbourhood -> EnvNeighbourhood
wrapNeighbourhood l w ns = map (wrap l w) ns

------------------------------------------------------------------------------------------------------------------------
-- 2D DISCRETE SPATIAL
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

neighbourhoodOf :: EnvCoord -> EnvNeighbourhood -> EnvNeighbourhood
neighbourhoodOf (x,y) ns = map (\(x', y') -> (x + x', y + y')) ns

neighbourhoodScale :: EnvNeighbourhood -> Int -> EnvNeighbourhood
neighbourhoodScale ns s = map (\(x,y) -> (x * s, y * s)) ns

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

