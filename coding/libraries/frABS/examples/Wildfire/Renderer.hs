module Wildfire.Renderer (
    renderWildfireFrame
  ) where

import Wildfire.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type WildfireRenderFrame = RenderFrame WildfireAgentState WildfireMsg WildfireCell WildfireLinkLabel
type WildfireAgentColorer = AgentCellColorer WildfireAgentState

renderWildfireFrame :: WildfireRenderFrame
renderWildfireFrame = render2dDiscreteFrame 
                            (defaultAgentRenderer wildfireAgentColor)
                            voidEnvironmentRenderer

wildfireAgentColor :: WildfireAgentColorer
wildfireAgentColor WildfireAgentState { wfLifeState = state, wfFuelCurr = fuel }
    | Living == state = GLO.makeColor (realToFrac 0.0) (realToFrac fuel) (realToFrac 0.0) 1.0
    | Burning == state = GLO.makeColor (realToFrac fuel) (realToFrac 0.0) (realToFrac 0.0) 1.0
    | Dead == state = GLO.greyN 0.5 