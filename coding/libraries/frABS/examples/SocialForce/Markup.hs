module SocialForce.Markup (
      Wall
    , wall
  ) where

import FRP.FrABS

type Wall = [Continuous2dCoord]

wall :: Continuous2dCoord -> [Continuous2dCoord] -> Wall
wall ref cs = ref : map (addCoord ref) cs 