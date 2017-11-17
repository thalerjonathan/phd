module SocialForce.Markup (
      Wall

    , nearestPointWall
  ) where

import FRP.FrABS

type Wall = (Continuous2dCoord, Continuous2dCoord)
type Rect = (Continuous2dCoord, Continuous2dDimension)

nearestPointWall :: Wall -> Continuous2dCoord -> (Double, Continuous2dCoord)
nearestPointWall (from, to) (x, y) = (0, (0, 0)) -- TODO: implement