module FrSIRSSpatial.Renderer (
    renderFrSIRSSpatialFrame
  ) where

import FrSIRSSpatial.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type FrSIRSSpatialRenderFrame = RenderFrame FrSIRSSpatialAgentState FrSIRSSpatialMsg FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentColorer = AgentCellColorerDisc2d FrSIRSSpatialAgentState

renderFrSIRSSpatialFrame :: FrSIRSSpatialRenderFrame
renderFrSIRSSpatialFrame = renderFrameDisc2d 
                                (defaultAgentRendererDisc2d frSIRSSpatialAgentColor sirsCoord)
                                voidEnvRendererDisc2d

frSIRSSpatialAgentColor :: FrSIRSSpatialAgentColorer
frSIRSSpatialAgentColor FrSIRSSpatialAgentState { sirsState = state } = frSIRSSpatialAgentColorAux state
    where   
        frSIRSSpatialAgentColorAux :: SIRSState -> GLO.Color
        frSIRSSpatialAgentColorAux Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
        frSIRSSpatialAgentColorAux Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
        frSIRSSpatialAgentColorAux Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0