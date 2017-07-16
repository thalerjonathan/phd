module FRP.FrABS.Rendering.Discrete2D (
    AgentRendererDisc2d,
    AgentCellColorer,
    AgentCoordDisc2d,
    EnvDisc2dRenderer,
    EnvDisc2dCellColorer,

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
 
type AgentRendererDisc2d s m c = (Float, Float) 
                                    -> (Int, Int) 
                                    -> AgentOut s m (Discrete2d c) 
                                    -> GLO.Picture
type AgentCellColorer s = s -> GLO.Color
type AgentCoordDisc2d s = (s -> Discrete2dCoord)

type EnvDisc2dRenderer c = (Float, Float) -> (Int, Int) -> (Discrete2dCoord, c) -> GLO.Picture
type EnvDisc2dCellColorer c = c -> GLO.Color

render2dDiscreteFrame :: AgentRendererDisc2d s m c
                            -> EnvDisc2dRenderer c
                            -> (Int, Int) 
                            -> [AgentOut s m (Discrete2d c)] 
                            -> Discrete2d c 
                            -> GLO.Picture
render2dDiscreteFrame ar er wSize@(wx, wy) aouts e = GLO.Pictures (envPics ++ agentPics)
    where
        (cx, cy) = envDisc2dDims e
        cellWidth = fromIntegral wx / fromIntegral cx
        cellHeight = fromIntegral wy / fromIntegral cy

        cells = allCellsWithCoords e

        agentPics = map (ar (cellWidth, cellHeight) wSize) aouts
        envPics = map (er (cellWidth, cellHeight) wSize) cells

defaultEnvironmentRenderer :: EnvDisc2dCellColorer c -> EnvDisc2dRenderer c
defaultEnvironmentRenderer cc (rectWidth, rectHeight) (wx, wy) ((x, y), cell) = GLO.Pictures [cellRect]
    where
        halfXSize = fromRational $ toRational wx / 2.0
        halfYSize = fromRational $ toRational wy / 2.0

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        color = cc cell

        cellRect = GLO.color color $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

voidEnvironmentRenderer :: EnvDisc2dRenderer ec
voidEnvironmentRenderer _ _ _ = GLO.Pictures []

defaultEnvironmentColorer :: GLO.Color -> EnvDisc2dCellColorer ec
defaultEnvironmentColorer color _ = color

defaultAgentRenderer :: AgentCellColorer s -> AgentCoordDisc2d s -> AgentRendererDisc2d s m c
defaultAgentRenderer acf apf (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        s = aoState a
        (x, y) = apf s
        color = acf s

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

voidAgentRenderer :: AgentRendererDisc2d s m c
voidAgentRenderer _ _ _ = GLO.Pictures []

defaultAgentColorer :: GLO.Color -> AgentCellColorer s
defaultAgentColorer color _ = color