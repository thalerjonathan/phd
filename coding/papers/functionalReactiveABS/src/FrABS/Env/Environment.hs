{-# LANGUAGE Arrows #-}
module FrABS.Env.Environment where

import FRP.Yampa

{-
an Environment is a container which contains Agents and allows them to move arround
    => assigns an agent a position within a space
    => allows an agent to query its neighbourhood within this space
    => allows an agent to change its location
    => has a signal-function which is invoked once per iteration to allow the environment to behave e.g. regrow ressources which were harvested by the agents
-}

-- TODO: continuous environment
-- TODO: can we generalize to higher dimensions?
-- TODO: graph environment: no geometrical space => no position but only neighbours

type EnvironmentBehaviour = SF Environment Environment
type EnvCoord = (Int, Int)
type EnvLimits = (Int, Int)
type EnvNeighbourhood = [(Int, Int)]
data EnvWrapping = ClipToMax | WrapHorizontal | WrapVertical | WrapBoth

data Environment = Environment {
    envBehaviour :: EnvironmentBehaviour,
    envLimits :: EnvLimits,
    envNeighbourhood :: EnvNeighbourhood,
    envWrapping :: EnvWrapping
}

wrapNeighbourhood :: EnvLimits -> EnvWrapping -> EnvNeighbourhood -> EnvNeighbourhood
wrapNeighbourhood l w ns = map (wrap l w) ns

wrap :: EnvLimits -> EnvWrapping -> EnvCoord -> EnvCoord
wrap (maxX, maxY) ClipToMax (x, y) = (min x maxX, min y maxY)
wrap (maxX, maxY) WrapHorizontal (x, y) = (min x (x - maxX - 1), y)
wrap (maxX, maxY) WrapVertical (x, y) = (x, min y (y - maxY - 1))
wrap (maxX, maxY) WrapBoth (x, y) = (min x (x - maxX - 1), min y (y - maxY - 1))

neighbourhoodOf :: EnvCoord -> EnvNeighbourhood -> EnvNeighbourhood
neighbourhoodOf (x,y) ns = map (\(x', y') -> (x + x', y + y')) ns

neumann :: EnvNeighbourhood
neumann = [top, left, right, bottom]

neumannSelf :: EnvNeighbourhood
neumannSelf = [top, left, center, right, bottom]

moore :: EnvNeighbourhood
moore = [topLeft, top, topRight,
         left, right,
         bottomLeft, bottom, bottomRight]

mooreSelf :: EnvNeighbourhood
mooreSelf = [topLeft, top, topRight,
             left, center, right,
             bottomLeft, bottom, bottomRight]

topLeft =       (-1, -1)
top =           ( 0, -1)
topRight =      ( 1, -1)
left =          (-1,  0)
center =        ( 0,  0)
right =         ( 1,  0)
bottomLeft =    (-1,  1)
bottom =        ( 0,  1)
bottomRight =   ( 1,  1)

