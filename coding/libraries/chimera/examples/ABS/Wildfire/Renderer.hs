module Renderer 
  (
    renderWildfireFrame
  ) where

import FRP.Chimera
import qualified Graphics.Gloss as GLO

import Model

type WildfireRenderFrame  = RenderFrame WildfireAgentState WildfireEnvironment
type WildfireAgentColorer = AgentCellColorerDisc2d WildfireAgentState

renderWildfireFrame :: WildfireRenderFrame
renderWildfireFrame = renderFrameDisc2d 
                        (defaultAgentRendererDisc2d wildfireAgentColor wfCoord)
                        voidEnvRendererDisc2d

wildfireAgentColor :: WildfireAgentColorer
wildfireAgentColor WildfireAgentState { wfLifeState = state, wfFuelCurr = fuel }
  | Living == state = GLO.makeColor 0.0 (realToFrac fuel) 0.0 1.0
  | Burning == state = GLO.makeColor (realToFrac fuel) 0.0 0.0 1.0
  | otherwise = GLO.greyN 0.5 