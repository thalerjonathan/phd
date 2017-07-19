module FRP.FrABS.Rendering.Discrete2d (
    AgentRendererDisc2d,
    AgentCellColorerDisc2d,
    AgentCoordDisc2d,
    EnvRendererDisc2d,
    EnvCellColorerDisc2d,

    renderFrameDisc2d,

    defaultEnvRendererDisc2d,
    defaultEnvColorerDisc2d,
    voidEnvRendererDisc2d,

    defaultAgentRendererDisc2d,
    defaultAgentColorerDisc2d,
    voidAgentRendererDisc2d
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Discrete

import qualified Graphics.Gloss as GLO
 
type AgentRendererDisc2d s = (Float, Float) 
                                -> (Int, Int) 
                                -> s
                                -> GLO.Picture
type AgentCellColorerDisc2d s = s -> GLO.Color
type AgentCoordDisc2d s = (s -> Discrete2dCoord)

type EnvRendererDisc2d c = (Float, Float) -> (Int, Int) -> (Discrete2dCoord, c) -> GLO.Picture
type EnvCellColorerDisc2d c = c -> GLO.Color

renderFrameDisc2d :: AgentRendererDisc2d s
                        -> EnvRendererDisc2d c
                        -> (Int, Int) 
                        -> [s]
                        -> Discrete2d c 
                        -> GLO.Picture
renderFrameDisc2d ar er wSize@(wx, wy) ss e = GLO.Pictures (envPics ++ agentPics)
    where
        (cx, cy) = envDisc2dDims e
        cellWidth = fromIntegral wx / fromIntegral cx
        cellHeight = fromIntegral wy / fromIntegral cy

        cells = allCellsWithCoords e

        agentPics = map (ar (cellWidth, cellHeight) wSize) ss
        envPics = map (er (cellWidth, cellHeight) wSize) cells -- TODO: is this expensive when we are doing void-rendering? should we use a maybe?

defaultEnvRendererDisc2d :: EnvCellColorerDisc2d c -> EnvRendererDisc2d c
defaultEnvRendererDisc2d cc (rectWidth, rectHeight) (wx, wy) ((x, y), cell) = GLO.Pictures [cellRect]
    where
        halfXSize = fromRational $ toRational wx / 2.0
        halfYSize = fromRational $ toRational wy / 2.0

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        color = cc cell

        cellRect = GLO.color color $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

voidEnvRendererDisc2d :: EnvRendererDisc2d ec
voidEnvRendererDisc2d _ _ _ = GLO.Blank

defaultEnvColorerDisc2d :: GLO.Color -> EnvCellColorerDisc2d ec
defaultEnvColorerDisc2d color _ = color

defaultAgentRendererDisc2d :: AgentCellColorerDisc2d s 
                                -> AgentCoordDisc2d s 
                                -> AgentRendererDisc2d s
defaultAgentRendererDisc2d acf apf (rectWidth, rectHeight) (wx, wy) s = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = apf s
        color = acf s

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

voidAgentRendererDisc2d :: AgentRendererDisc2d s
voidAgentRendererDisc2d _ _ _ = GLO.Blank

defaultAgentColorerDisc2d :: GLO.Color -> AgentCellColorerDisc2d s
defaultAgentColorerDisc2d color _ = color