module SIRS.Renderer (
    renderSIRSFrame
  ) where

import SIRS.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type SIRSRenderFrame = RenderFrame SIRSAgentState SIRSMsg SIRSEnvCell SIRSEnvLink
type SIRSAgentColorer = AgentCellColorer SIRSAgentState

renderSIRSFrame :: SIRSRenderFrame
renderSIRSFrame = render2dDiscreteFrame 
                    (defaultAgentRenderer sirsAgentColor)
                    voidEnvironmentRenderer

sirsAgentColor :: SIRSAgentColorer
sirsAgentColor SIRSAgentState {sirsState = state} = sirsAgentColorAux state
    where
        sirsAgentColorAux :: SIRSState -> GLO.Color
        sirsAgentColorAux Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
        sirsAgentColorAux Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
        sirsAgentColorAux Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0