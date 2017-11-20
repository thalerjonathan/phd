module SIR.Renderer (
    renderSIRFrame
  ) where

import SIR.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type SIRRenderFrame = RenderFrame SIRAgentState SIREnvironment
type SIRAgentColorer = AgentCellColorerDisc2d SIRAgentState

renderSIRFrame :: SIRRenderFrame
renderSIRFrame = renderFrameDisc2d 
                    (defaultAgentRendererDisc2d sirAgentColor sirCoord)
                    voidEnvRendererDisc2d

sirAgentColor :: SIRAgentColorer
sirAgentColor SIRAgentState {sirState = state} = sirAgentColorAux state
    where
        sirAgentColorAux :: SIRState -> GLO.Color
        sirAgentColorAux Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
        sirAgentColorAux Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
        sirAgentColorAux Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0