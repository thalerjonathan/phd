module FrSIRSNetwork.Renderer (
    renderFrSIRSNetworkFrame
  ) where

import FrSIRSNetwork.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type FrSIRSNetworkRenderFrame = RenderFrame FrSIRSNetworkAgentState FrSIRSNetworkMsg FrSIRSNetworkEnvCell FrSIRSNetworkEnvLink
type FrSIRSNetworkAgentColorer = AgentCellColorer FrSIRSNetworkAgentState

renderFrSIRSNetworkFrame :: FrSIRSNetworkRenderFrame
renderFrSIRSNetworkFrame = render2dDiscreteFrame 
                                (defaultAgentRenderer frSIRSNetworkAgentColor)
                                voidEnvironmentRenderer

frSIRSNetworkAgentColor :: FrSIRSNetworkAgentColorer
frSIRSNetworkAgentColor Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
frSIRSNetworkAgentColor Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
frSIRSNetworkAgentColor Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0
