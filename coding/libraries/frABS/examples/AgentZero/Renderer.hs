module AgentZero.Renderer (
    renderAgentZeroFrame
  ) where

import           FRP.FrABS

import           AgentZero.Model

import qualified Graphics.Gloss  as GLO

type AgentZeroRenderFrame = RenderFrame AgentZeroAgentState AgentZeroEnvironment 
type AgentZeroEnvCellColorer = EnvCellColorerDisc2d AgentZeroEnvCell
type AgentZeroCoord = AgentCoordDisc2d AgentZeroAgentState

agentSize = 15
agentColor = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

renderAgentZeroFrame :: AgentZeroRenderFrame
renderAgentZeroFrame wSize@(wx, wy) ss e = GLO.Pictures [patchesPic, agentsPic]
    where
        wp = azWorldPatches e
        as = azAgentSpace e

        patchesPic = renderFrameDisc2d 
                        voidAgentRendererDisc2d --(defaultAgentRendererDisc2d (defaultAgentColorerDisc2d agentColor) agentZeroDiscCoord) -- voidAgentRendererDisc2d
                        (defaultEnvRendererDisc2d agentZeroEnvCellColor)
                        wSize
                        [] -- ss
                        wp

        agentsPic = renderFrameCont2d 
                        (defaultAgentRendererCont2d agentSize (defaultAgentColorerCont2d agentColor) azAgentCoord)
                        voidEnvRendererCont2d
                        wSize
                        ss
                        as

agentZeroDiscCoord :: AgentZeroCoord
agentZeroDiscCoord AgentZeroAgentState { azAgentCoord = coordCont2d } = (xd, yd)
    where
        (xc, yc) = coordCont2d
        xd = floor xc
        yd = floor yc

agentZeroEnvCellColor :: AgentZeroEnvCellColorer
agentZeroEnvCellColor AgentZeroEnvCell { azCellState = state, azCellShade = shade }
    | Friendly == state = GLO.makeColor (realToFrac 1.0) (realToFrac 1.0) (realToFrac shade) 1.0
    | Attack == state = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | Dead == state = GLO.black