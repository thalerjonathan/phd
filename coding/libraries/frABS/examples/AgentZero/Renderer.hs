module AgentZero.Renderer (
    renderAgentZeroFrame
  ) where

import           FRP.FrABS

import           AgentZero.Model

import qualified Graphics.Gloss  as GLO

type AgentZeroRenderFrame = RenderFrame AgentZeroAgentState AgentZeroEnvironment 
type AgentZeroEnvCellColorer = EnvCellColorerDisc2d AgentZeroEnvCell
type AgentZeroCoord = AgentCoordDisc2d AgentZeroAgentState

agentColor = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

renderAgentZeroFrame :: AgentZeroRenderFrame
renderAgentZeroFrame wSize@(wx, wy) ss e = 
        renderFrameDisc2d 
            (defaultAgentRendererDisc2d (defaultAgentColorerDisc2d agentColor) agentZeroCoord) -- voidAgentRendererDisc2d
            (defaultEnvRendererDisc2d agentZeroEnvCellColor)
            wSize
            ss
            wp
    where
        wp = azWorldPatches e

agentZeroCoord :: AgentZeroCoord
agentZeroCoord AgentZeroAgentState { azAgentCoord = coordCont2d } = (xd, yd)
    where
        (xc, yc) = coordCont2d
        xd = floor xc
        yd = floor yc

agentZeroEnvCellColor :: AgentZeroEnvCellColorer
agentZeroEnvCellColor AgentZeroEnvCell { azCellState = state, azCellShade = shade }
    | Friendly == state = GLO.makeColor (realToFrac 1.0) (realToFrac 1.0) (realToFrac shade) 1.0
    | Attack == state = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | Dead == state = GLO.black