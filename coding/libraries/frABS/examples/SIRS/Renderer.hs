module SIRS.Renderer (
    renderFrame
  ) where

import SIRS.Model

import FrABS.Agent.Agent
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

renderFrame :: (Int, Int) -> [SIRSAgentOut] -> SIRSEnvironment -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> SIRSAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColor (sirsState $ aoState a)

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

agentColor :: SIRSState -> GLO.Color
agentColor Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
agentColor Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
agentColor Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0