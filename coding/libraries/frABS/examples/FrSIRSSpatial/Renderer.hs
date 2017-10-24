module FrSIRSSpatial.Renderer (
    renderFrSIRSSpatialFrame
  ) where

import FrSIRSSpatial.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type FrSIRSSpatialRenderFrame = RenderFrame FrSIRSSpatialAgentState FrSIRSSpatialEnvironment
type FrSIRSSpatialAgentColorer = AgentCellColorerDisc2d FrSIRSSpatialAgentState

renderFrSIRSSpatialFrame :: FrSIRSSpatialRenderFrame
renderFrSIRSSpatialFrame wSize@(wx, wy) t ss e = GLO.Pictures $ (agentPics ++ [timeStepTxt])
  where
      (GLO.Pictures agentPics) = renderFrameDisc2d 
          (defaultAgentRendererDisc2d frSIRSSpatialAgentColor sirsCoord)
          voidEnvRendererDisc2d
          wSize
          t
          ss
          e

      timeStepTxt = GLO.color GLO.white $ GLO.translate (-halfWSizeX - 30) (halfWSizeY) $ GLO.scale 0.1 0.1 $ GLO.Text (show t)
      halfWSizeX = fromIntegral wx / 2.0 
      halfWSizeY = fromIntegral wy / 2.0 

frSIRSSpatialAgentColor :: FrSIRSSpatialAgentColorer
frSIRSSpatialAgentColor FrSIRSSpatialAgentState { sirsState = state } = frSIRSSpatialAgentColorAux state
    where   
        frSIRSSpatialAgentColorAux :: SIRSState -> GLO.Color
        frSIRSSpatialAgentColorAux Susceptible = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac 0.7) 1.0 
        frSIRSSpatialAgentColorAux Infected = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0
        frSIRSSpatialAgentColorAux Recovered = GLO.makeColor (realToFrac 0.0) (realToFrac 0.55) (realToFrac 0.0) 1.0