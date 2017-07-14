module FRP.FrABS.Environment.Continuous where

  ) where

import FRP.Yampa
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List

import Control.Monad.Random
import Control.Monad.Trans.State

type EnvDim d = (d, d)
type EnvCoord d = EnvDim d
type EnvLimits d = EnvDim d

type EnvironmentBehaviour l d = SF (Environment l d) (Environment l d)
data EnvWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

type EnvGraph l = Gr () l

data Environment l d = Environment {
    envBehaviour :: Maybe (EnvironmentBehaviour l d),
    envLimits :: EnvDim d,
    envWrapping :: EnvDim d,
    envRng :: StdGen,
    envGraph :: EnvGraph l
}

createEnvironment :: Maybe (EnvironmentBehaviour l d)
                        -> EnvLimits
                        -> EnvWrapping
                        -> StdGen
                        -> Maybe (EnvGraph l)
                        -> Environment l d
createEnvironment beh l w cs rng mayGr = Environment {
                                            envBehaviour = beh,
                                            envLimits = l,
                                            envWrapping = w,
                                            envRng = rng,
                                            envGraph = maybe empty id mayGr
                                        }

neighbourEdges :: Node -> Environment l ->  [l]
neighbourEdges node env = map fst ls
    where
        ls = neighbourLinks node env

neighbourNodes :: Node -> Environment l -> [Node]
neighbourNodes node env = map snd ls
    where
        ls = neighbourLinks node env

neighbourNodesM :: Node -> State (Environment l) [Node]
neighbourNodesM node = state (\env -> (neighbourNodes node env, env))

neighbourLinks :: Node -> Environment l -> Adj l
neighbourLinks node env = lneighbors gr node
    where
        gr = envGraph env

directLinkBetween :: Node -> Node -> Environment l -> Maybe l
directLinkBetween n1 n2 env = 
    do
        let ls = neighbourLinks n1 env
        (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
        return linkLabel

directLinkBetweenM :: Node -> Node -> State (Environment l) (Maybe l)
directLinkBetweenM n1 n2 = state (\env -> (directLinkBetween n1 n2 env, env))

distanceManhattan :: EnvCoord -> EnvCoord -> Int
distanceManhattan (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclidean :: EnvCoord -> EnvCoord -> Double
distanceEuclidean (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = x2 - x1
        yDelta = y2 - y1

wrap :: EnvLimits -> EnvWrapping -> EnvCoord -> EnvCoord
wrap (maxX, maxY) ClipToMax (x, y) = (max 0 (min x (maxX - 1)), max 0 (min y (maxY - 1))) -- TODO: THIS WORKS FOR DISCRETE ONLY
wrap l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrap l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrap l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrap l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrap l WrapVertical (x, y + maxY)
    | y >= maxY = wrap l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrap l WrapBoth coord = wrap l WrapHorizontal $ wrap l WrapVertical coord