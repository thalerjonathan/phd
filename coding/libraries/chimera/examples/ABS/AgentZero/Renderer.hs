module Renderer 
  (
    renderAgentZeroFrame
  ) where

import FRP.Chimera
import qualified Graphics.Gloss  as GLO

import Model

type AgentZeroRenderFrame     = RenderFrame AgentZeroAgentState AgentZeroEnvironment 
type AgentZeroEnvCellColorer  = EnvCellColorerDisc2d AgentZeroEnvCell
type AgentZeroCoord           = AgentCoordDisc2d AgentZeroAgentState

agentSize :: Float
agentSize = 15

agentColor :: GLO.Color
agentColor = GLO.makeColor 0.0 0.3 0.6 1.0

renderAgentZeroFrame :: AgentZeroRenderFrame
renderAgentZeroFrame wSize t ss e = GLO.Pictures [patchesPic, agentsPic]
  where
    wp = azWorldPatches e
    as = azAgentSpace e

    patchesPic = renderFrameDisc2d 
                    voidAgentRendererDisc2d --(defaultAgentRendererDisc2d (defaultAgentColorerDisc2d agentColor) agentZeroDiscCoord) -- voidAgentRendererDisc2d
                    (defaultEnvRendererDisc2d agentZeroEnvCellColor)
                    wSize
                    t
                    [] -- ss
                    wp

    agentsPic = renderFrameCont2d 
                    (defaultAgentRendererCont2d agentSize (defaultAgentColorerCont2d agentColor) azAgentCoord)
                    voidEnvRendererCont2d
                    wSize
                    t
                    ss
                    as

_agentZeroDiscCoord :: AgentZeroCoord
_agentZeroDiscCoord AgentZeroAgentState { azAgentCoord = coordCont2d } = (xd, yd)
  where
    (xc, yc) = coordCont2d
    xd = floor xc
    yd = floor yc

agentZeroEnvCellColor :: AgentZeroEnvCellColorer
agentZeroEnvCellColor AgentZeroEnvCell { azCellState = state, azCellShade = shade }
  | Friendly == state = GLO.makeColor 1.0 1.0 shade 1.0
  | Attack == state = GLO.makeColor 1.0 0.1 0.1 1.0
  | otherwise = GLO.black