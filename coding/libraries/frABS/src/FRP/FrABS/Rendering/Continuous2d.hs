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
import FRP.FrABS.Rendering.GlossSimulator

import qualified Graphics.Gloss as GLO
 
type AgentRendererCont2d s = Continuous2DDimension
                                -> (Int, Int)
                                -> (AgentId, s)
                                -> GLO.Picture
type AgentColorerCont2d s = s -> GLO.Color
type AgentCoordCont2d s = (s -> Continuous2DCoord)

type EnvRendererCont2d = (Int, Int) -> Continuous2d -> GLO.Picture

renderFrameCont2d :: AgentRendererCont2d s
                        -> EnvRendererCont2d
                        -> RenderFrame s Continuous2d
renderFrameCont2d ar er winSize@(wx, wy) ss e = GLO.Pictures (envPic : agentPics)
    where
        (dx, dy) = envCont2dDims e

        scaleX = fromIntegral wx / dx 
        scaleY = fromIntegral wy / dy

        agentPics = map (ar (scaleX, scaleY) winSize) ss
        envPic = er winSize e

defaultEnvRendererCont2d :: EnvRendererCont2d
defaultEnvRendererCont2d = voidEnvRendererCont2d

voidEnvRendererCont2d :: EnvRendererCont2d
voidEnvRendererCont2d _ _ = GLO.Blank

defaultAgentRendererCont2d :: Float -> AgentColorerCont2d s -> AgentCoordCont2d s -> AgentRendererCont2d s
defaultAgentRendererCont2d size acf apf (sx, sy) (wx, wy) (_, s) = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 size
    where
        (x, y) = apf s
        color = acf s

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (x * sx)) - halfXSize
        yPix = fromRational (toRational (y * sy)) - halfYSize

voidAgentRendererCont2d :: AgentRendererCont2d s
voidAgentRendererCont2d _ _ _ = GLO.Blank

defaultAgentColorerCont2d :: GLO.Color -> AgentColorerCont2d s
defaultAgentColorerCont2d color _ = color