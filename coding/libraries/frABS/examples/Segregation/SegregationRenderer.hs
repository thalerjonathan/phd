module Segregation.SegregationRenderer (
    renderFrame
  ) where

import Segregation.SegregationModel

import FrABS.Agent.Agent
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

renderFrame :: (Int, Int) -> [SegAgentOut] -> SegEnvironment -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> SegAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor (segParty $ aoState a)

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

agentColor :: SegParty -> GLO.Color
agentColor Red = GLO.makeColor (realToFrac 0.6) (realToFrac 0.0) (realToFrac 0.0) 1.0
agentColor Green = GLO.makeColor (realToFrac 0.0) (realToFrac 0.6) (realToFrac 0.0) 1.0
