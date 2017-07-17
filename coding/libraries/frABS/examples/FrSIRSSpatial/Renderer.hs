module FrSIRSSpatial.Renderer (
    renderFrSIRSSpatialFrame
  ) where

import FrSIRSSpatial.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type FrSIRSSpatialRenderFrame = RenderFrame FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentColorer = AgentCellColorer FrSIRSSpatialAgentState

renderFrSIRSSpatialFrame :: FrSIRSSpatialRenderFrame
renderFrSIRSSpatialFrame = render2dDiscreteFrame 
                                (defaultAgentRenderer frSIRSSpatialAgentColor sirsCoord)
                                voidEnvironmentRenderer

frSIRSSpatialAgentColor :: FrSIRSSpatialAgentColorer
frSIRSSpatialAgentColor FrSIRSSpatialAgentState { sirsState = state } = frSIRSSpatialAgentColorAux state
    where   
        frSIRSSpatialAgentColorAux :: SIRSState -> GLO.Color
        frSIRSSpatialAgentColorAux Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
        frSIRSSpatialAgentColorAux Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
        frSIRSSpatialAgentColorAux Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0