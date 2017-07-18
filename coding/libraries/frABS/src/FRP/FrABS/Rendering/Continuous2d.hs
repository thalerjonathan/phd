module FRP.FrABS.Rendering.Continuous2d (
    AgentRendererCont2d,
    AgentColorerCont2d,
    AgentCoordCont2d,
    EnvRendererCont2d,

    renderFrameCont2d,

    defaultEnvRendererCont2d,
    voidEnvRendererCont2d,

    defaultAgentRendererCont2d,
    defaultAgentColorerCont2d,
    voidAgentRendererCont2d
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Continuous

import qualified Graphics.Gloss as GLO
 
type AgentRendererCont2d s m c = Continuous2DDimension
                                    -> (Int, Int)
                                    -> AgentOut s m Continuous2d
                                    -> GLO.Picture
type AgentColorerCont2d s = s -> GLO.Color
type AgentCoordCont2d s = (s -> Continuous2DCoord)

type EnvRendererCont2d = (Int, Int) -> Continuous2d -> GLO.Picture

renderFrameCont2d :: AgentRendererCont2d s m c
                        -> EnvRendererCont2d
                        -> (Int, Int) 
                        -> [AgentOut s m Continuous2d] 
                        -> Continuous2d
                        -> GLO.Picture
renderFrameCont2d ar er winSize@(wx, wy) aouts e = GLO.Pictures (envPic : agentPics)
    where
        (dx, dy) = envCont2dDims e

        scaleX = fromIntegral wx / dx 
        scaleY = fromIntegral wy / dy

        agentPics = map (ar (scaleX, scaleY) winSize) aouts
        envPic = er winSize e

defaultEnvRendererCont2d :: EnvRendererCont2d
defaultEnvRendererCont2d = voidEnvRendererCont2d

voidEnvRendererCont2d :: EnvRendererCont2d
voidEnvRendererCont2d _ _ = GLO.Blank

defaultAgentRendererCont2d :: Float -> AgentColorerCont2d s -> AgentCoordCont2d s -> AgentRendererCont2d s m c
defaultAgentRendererCont2d size acf apf (sx, sy) (wx, wy) ao = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 size
    where
        s = aoState ao
        (x, y) = apf s
        color = acf s

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (x * sx)) - halfXSize
        yPix = fromRational (toRational (y * sy)) - halfYSize

voidAgentRendererCont2d :: AgentRendererCont2d s m c
voidAgentRendererCont2d _ _ _ = GLO.Pictures []

defaultAgentColorerCont2d :: GLO.Color -> AgentColorerCont2d s
defaultAgentColorerCont2d color _ = color