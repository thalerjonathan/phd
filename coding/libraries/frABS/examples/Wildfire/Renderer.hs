module Wildfire.Renderer (
    renderFrame
  ) where

import Wildfire.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

renderFrame :: (Int, Int) -> [WildfireAgentOut] -> WildfireEnvironment -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> WildfireAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix  $ GLO.rectangleSolid rectWidth rectWidth -- $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        
agentColor :: WildfireAgentState -> GLO.Color
agentColor WildfireAgentState { wfLifeState = state, wfFuelCurr = fuel }
    | Living == state = GLO.makeColor (realToFrac 0.0) (realToFrac fuel) (realToFrac 0.0) 1.0
    | Burning == state = GLO.makeColor (realToFrac fuel) (realToFrac 0.0) (realToFrac 0.0) 1.0
    | Dead == state = GLO.greyN 0.5 