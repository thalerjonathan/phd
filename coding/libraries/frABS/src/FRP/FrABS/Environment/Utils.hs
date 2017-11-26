module FRP.FrABS.Environment.Utils
  (
    cont2dToDisc2d
  , disc2dToCont2d

  , cont2dTransDisc2d
  , disc2dTransCont2d
  ) where

import FRP.FrABS.Environment.Continuous
import FRP.FrABS.Environment.Discrete

cont2dToDisc2d :: Continuous2dCoord -> Discrete2dCoord
cont2dToDisc2d (xc, yc) = (floor xc, floor yc)

disc2dToCont2d ::  Discrete2dCoord -> Continuous2dCoord
disc2dToCont2d (xd, yd) = (fromIntegral xd, fromIntegral yd)

cont2dTransDisc2d :: Discrete2d c -> Continuous2d o -> Continuous2dCoord -> Discrete2dCoord
cont2dTransDisc2d ed ec (xc, yc) = wrapDisc2dEnv ed $ cont2dToDisc2d (xt, yt)
  where
    (ddx, ddy) = envDisc2dDims ed
    (dcx, dcy) = envCont2dDims ec

    rx = fromIntegral ddx / dcx
    ry = fromIntegral ddy / dcy

    xt = xc * rx
    yt = yc * ry

disc2dTransCont2d :: Continuous2d o -> Discrete2d c -> Discrete2dCoord -> Continuous2dCoord
disc2dTransCont2d ec ed (xd, yd) = wrapCont2dEnv ec (xc, yc)
  where
    (ddx, ddy) = envDisc2dDims ed
    (dcx, dcy) = envCont2dDims ec

    rx = fromIntegral ddx / dcx
    ry = fromIntegral ddy / dcy

    xc = fromIntegral xd / rx
    yc = fromIntegral yd / ry