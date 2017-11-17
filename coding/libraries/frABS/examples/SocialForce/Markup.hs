module SocialForce.Markup (
      Line
    , Rect

    , line
    , nearestPointLine
  ) where

import FRP.FrABS

type Line = (Continuous2dCoord, Continuous2dCoord)
type Rect = (Continuous2dCoord, Double, Double)

line :: Continuous2dCoord -> Continuous2dCoord -> Line
line from to = (from, to)

-- TODO: maybe consider implementing a type-class Markup
nearestPointLine :: Line -> Continuous2dCoord -> (Double, Continuous2dCoord)
nearestPointLine (from, to) c = (dist, proj)
  where
    vft@(x, y) = vecFromCoords from to
    dvp = dotCoords vft c
    dvv = dotCoords vft vft
    ddiv = dvp / dvv
    proj = (x * ddiv, y * ddiv)
    dist = distanceEuclideanCont2d proj c