module FRP.FrABS.Rendering.Discrete2D (
    AgentRenderer,
    AgentCellColorer,
    EnvironmentRenderer,
    EnvironmentCellColorer,

    render2dDiscreteFrame,

    defaultEnvironmentRenderer,
    defaultEnvironmentColorer,
    voidEnvironmentRenderer,

    defaultAgentRenderer,
    defaultAgentColorer,
    voidAgentRenderer
  ) where
  
import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Discrete

import qualified Graphics.Gloss as GLO
 
-- NOTE: 2d cell-size, window-size, agent 
type AgentRenderer s m e = (Float, Float) -> (Int, Int) -> AgentOut s m e -> GLO.Picture
type AgentCellColorer s = s -> GLO.Color
-- NOTE: 2d cell-size, window-size, environment-coord 
type EnvironmentRenderer c = (Float, Float) -> (Int, Int) -> (EnvCoord, c) -> GLO.Picture
type EnvironmentCellColorer c = c -> GLO.Color

render2dDiscreteFrame :: AgentRenderer s m (EnvironmentDiscrete2D e c)
                            -> EnvironmentRenderer c
                            -> (Int, Int) 
                            -> [AgentOut s m e] 
                            -> e 
                            -> GLO.Picture
render2dDiscreteFrame ar er wSize@(wx, wy) aouts env = GLO.Pictures (envPics ++ agentPics)
    where
        (cx, cy) = environmentLimits env
        cellWidth = fromIntegral wx / fromIntegral cx
        cellHeight = fromIntegral wy / fromIntegral cy

        cells = allCellsWithCoords env

        agentPics = map (ar (cellWidth, cellHeight) wSize) aouts
        envPics = map (er (cellWidth, cellHeight) wSize) cells

defaultEnvironmentRenderer :: EnvironmentCellColorer c -> EnvironmentRenderer c
defaultEnvironmentRenderer cc (rectWidth, rectHeight) (wx, wy) ((x, y), cell) = GLO.Pictures [cellRect]
    where
        halfXSize = fromRational $ toRational wx / 2.0
        halfYSize = fromRational $ toRational wy / 2.0

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        color = cc cell

        cellRect = GLO.color color $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

voidEnvironmentRenderer :: EnvironmentRenderer ec
voidEnvironmentRenderer _ _ _ = GLO.Pictures []

defaultEnvironmentColorer :: GLO.Color -> EnvironmentCellColorer ec
defaultEnvironmentColorer color _ = color

defaultAgentRenderer :: AgentCellColorer s -> AgentRenderer s m e
defaultAgentRenderer ac (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = ac $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

voidAgentRenderer :: AgentRenderer s m e
voidAgentRenderer _ _ _ = GLO.Pictures []

defaultAgentColorer :: GLO.Color -> AgentCellColorer s
defaultAgentColorer color _ = color