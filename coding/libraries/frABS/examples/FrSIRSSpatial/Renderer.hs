module FrSIRSSpatial.Renderer (
    renderFrSIRSSpatialFrame
  ) where

import FrSIRSSpatial.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type FrSIRSSpatialRenderFrame = RenderFrame FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvCell FrSIRSSpatialEnvLink
type FrSIRSSpatialAgentColorer = AgentCellColorer FrSIRSSpatialAgentState

renderFrSIRSSpatialFrame :: FrSIRSSpatialRenderFrame
renderFrSIRSSpatialFrame = render2dDiscreteFrame 
                                (defaultAgentRenderer frSIRSSpatialAgentColor)
                                voidEnvironmentRenderer

frSIRSSpatialAgentColor :: FrSIRSSpatialAgentColorer
frSIRSSpatialAgentColor Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
frSIRSSpatialAgentColor Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
frSIRSSpatialAgentColor Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0
