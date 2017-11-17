module SocialForce.Markup (
      Line
    , Rect

    , line
    , nearestPointLine
  ) where

import FRP.FrABS

type Line = (Continuous2dCoord, Continuous2dCoord)
type Rect = (Continuous2dCoord, Continuous2dDimension)

line :: Continuous2dCoord -> Continuous2dCoord -> Line
line from to = (from, to)

-- TODO: maybe consider implementing a type-class Markup
nearestPointLine :: Line -> Continuous2dCoord -> (Double, Continuous2dCoord)
nearestPointLine (from, to) (x, y) = (0, (0, 0)) -- TODO: implement