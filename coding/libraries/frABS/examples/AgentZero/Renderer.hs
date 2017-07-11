module AgentZero.Renderer (
    renderFrame
  ) where

import           FRP.FrABS

import           AgentZero.Model

import qualified Graphics.Gloss  as GLO

renderFrame :: (Int, Int) -> [AgentZeroAgentOut] -> AgentZeroEnvironment -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures (envPics ++ agentPics)
    where
        (cx, cy) = envLimits env
        cellWidth = fromIntegral wx / fromIntegral cx
        cellHeight = fromIntegral wy / fromIntegral cy

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts
        envPics = map (renderEnvCell (cellWidth, cellHeight) wSize) cells

renderEnvCell :: (Float, Float)
                    -> (Int, Int)
                    -> (EnvCoord, AgentZeroEnvCell)
                    -> GLO.Picture
renderEnvCell (rectWidth, rectHeight) (wx, wy) ((x, y), cell) = GLO.Pictures [cellRect]
    where
        halfXSize = fromRational $ toRational wx / 2.0
        halfYSize = fromRational $ toRational wy / 2.0

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        c = cellColor cell

        cellRect = GLO.color c $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

cellColor :: AgentZeroEnvCell -> GLO.Color
cellColor AgentZeroEnvCell {azCellState = state, azCellShade = shade}
    | Friendly == state = GLO.makeColor (realToFrac 1.0) (realToFrac 1.0) (realToFrac shade) 1.0
    | Attack == state = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | Dead == state = GLO.black

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> AgentZeroAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
