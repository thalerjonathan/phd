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
type AgentRenderer s m ec l = (Float, Float) -> (Int, Int) -> AgentOut s m ec l -> GLO.Picture
type AgentCellColorer s = s -> GLO.Color
-- NOTE: 2d cell-size, window-size, environment-coord 
type EnvironmentRenderer ec = (Float, Float) -> (Int, Int) -> (EnvCoord, ec) -> GLO.Picture
type EnvironmentCellColorer ec = ec -> GLO.Color

render2dDiscreteFrame :: AgentRenderer s m ec l
                        -> EnvironmentRenderer ec
                        -> (Int, Int) 
                        -> [AgentOut s m ec l] 
                        -> Environment ec l 
                        -> GLO.Picture
render2dDiscreteFrame ar er wSize@(wx, wy) aouts env = GLO.Pictures (envPics ++ agentPics)
    where
        (cx, cy) = envLimits env
        cellWidth = fromIntegral wx / fromIntegral cx
        cellHeight = fromIntegral wy / fromIntegral cy

        cells = allCellsWithCoords env

        agentPics = map (ar (cellWidth, cellHeight) wSize) aouts
        envPics = map (er (cellWidth, cellHeight) wSize) cells

defaultEnvironmentRenderer :: EnvironmentCellColorer ec -> EnvironmentRenderer ec
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
defaultEnvironmentColorer c _ = c

defaultAgentRenderer :: AgentCellColorer s -> AgentRenderer s m ec l
defaultAgentRenderer ac (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = ac $ aoState a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

voidAgentRenderer :: AgentRenderer s m ec l
voidAgentRenderer _ _ _ = GLO.Pictures []

defaultAgentColorer :: GLO.Color -> AgentCellColorer s
defaultAgentColorer c _ = c