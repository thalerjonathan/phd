module Segregation.Renderer (
    renderSegFrame
  ) where

import Segregation.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type SegRenderFrame = RenderFrame SegAgentState SegMsg SegEnvCell SegEnvLink
type SegAgentColorer = AgentCellColorer SegAgentState

renderSegFrame :: SegRenderFrame
renderSegFrame = render2dDiscreteFrame 
                    (defaultAgentRenderer segAgentColor)
                    voidEnvironmentRenderer

segAgentColor :: SegAgentColorer
segAgentColor SegAgentState {segParty = party} = segAgentColorAux party
    where
        segAgentColorAux :: SegParty -> GLO.Color
        segAgentColorAux Red = GLO.makeColor (realToFrac 0.6) (realToFrac 0.0) (realToFrac 0.0) 1.0
        segAgentColorAux Green = GLO.makeColor (realToFrac 0.0) (realToFrac 0.6) (realToFrac 0.0) 1.0