module FRP.Chimera.Rendering.Discrete2d
  (
    AgentRendererDisc2d
  , AgentCellColorerDisc2d
  , AgentCoordDisc2d
  , EnvRendererDisc2d
  , EnvCellColorerDisc2d

  , renderFrameDisc2d

  , defaultEnvRendererDisc2d
  , defaultEnvColorerDisc2d
  , voidEnvRendererDisc2d

  , defaultAgentRendererDisc2d
  , defaultAgentColorerDisc2d
  , voidAgentRendererDisc2d

  , transformToWindow
  ) where

import FRP.Yampa
import qualified Graphics.Gloss as GLO

import FRP.Chimera.Agent.Agent
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Rendering.GlossSimulator
 
type AgentRendererDisc2d s      = (Float, Float) 
                                  -> (Int, Int) 
                                  -> Time
                                  -> (AgentId, s)
                                  -> GLO.Picture
type AgentCellColorerDisc2d s   = s -> GLO.Color
type AgentCoordDisc2d s         = (s -> Discrete2dCoord)

type EnvRendererDisc2d c        = (Float, Float) -> (Int, Int) -> Time -> (Discrete2dCoord, c) -> GLO.Picture
type EnvCellColorerDisc2d c     = c -> GLO.Color

renderFrameDisc2d :: AgentRendererDisc2d s
                    -> EnvRendererDisc2d c
                    -> RenderFrame s (Discrete2d c)
renderFrameDisc2d ar er wSize@(wx, wy) t ss e = GLO.Pictures (envPics ++ agentPics)
  where
    (cx, cy) = envDisc2dDims e
    cellWidth = fromIntegral wx / fromIntegral cx
    cellHeight = fromIntegral wy / fromIntegral cy

    cells = allCellsWithCoords e

    agentPics = map (ar (cellWidth, cellHeight) wSize t) ss
    envPics = map (er (cellWidth, cellHeight) wSize t) cells -- TODO: is this expensive when we are doing void-rendering? should we use a maybe?

defaultEnvRendererDisc2d :: EnvCellColorerDisc2d c -> EnvRendererDisc2d c
defaultEnvRendererDisc2d cc r@(rw, rh) w _t (coord, cell) = cellRect
  where
    (x, y) = transformToWindow r w coord
    color = cc cell
    cellRect = GLO.color color $ GLO.translate x y $ GLO.rectangleSolid rw rh

voidEnvRendererDisc2d :: EnvRendererDisc2d ec
voidEnvRendererDisc2d _ _ _ _ = GLO.Blank

defaultEnvColorerDisc2d :: GLO.Color -> EnvCellColorerDisc2d ec
defaultEnvColorerDisc2d color _ = color

defaultAgentRendererDisc2d :: AgentCellColorerDisc2d s 
                              -> AgentCoordDisc2d s 
                              -> AgentRendererDisc2d s
defaultAgentRendererDisc2d acf apf r@(rw, _) w _t (_, s) = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 rw
  where
    coord = apf s
    color = acf s
    (x, y) = transformToWindow r w coord

voidAgentRendererDisc2d :: AgentRendererDisc2d s
voidAgentRendererDisc2d _ _ _ _ = GLO.Blank

defaultAgentColorerDisc2d :: GLO.Color -> AgentCellColorerDisc2d s
defaultAgentColorerDisc2d color _ = color

transformToWindow :: (Float, Float)
                      -> (Int, Int) 
                      -> Discrete2dCoord 
                      -> (Float, Float)
transformToWindow (rw, rh) (wx, wy) (x, y) = (x', y')
  where
    halfXSize = fromRational (toRational wx / 2.0)
    halfYSize = fromRational (toRational wy / 2.0)

    x' = fromRational (toRational (fromIntegral x * rw)) - halfXSize
    y' = fromRational (toRational (fromIntegral y * rh)) - halfYSize