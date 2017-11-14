module SocialForce.Markup (
      Wall
    , wall

    , nearestPointWall
  ) where

import FRP.FrABS

type Wall = [Continuous2dCoord]

wall :: Continuous2dCoord -> [Continuous2dCoord] -> Wall
wall ref cs = ref : map (addCoord ref) cs 

nearestPointWall :: Wall -> Continuous2dCoord -> (Double, Continuous2dCoord)
nearestPointWall w (x, y) = (0, (0, 0)) -- TODO: implement