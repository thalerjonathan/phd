module FRP.FrABS.Rendering.Network (
    AgentRendererNetwork,
    AgentColorerNetwork,

    renderFrameNetwork,

    defaultAgentRendererNetwork,
    defaultAgentColorerNetwork
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Network

import qualified Graphics.Gloss as GLO

-- TODO: implement

type AgentRendererNetwork s l = (Float, Float)
                                -> Float
                                -> s 
                                -> l
                                -> GLO.Picture
type AgentColorerNetwork s = s -> GLO.Color

renderFrameNetwork :: AgentRendererNetwork s l
                        -> (Int, Int) 
                        -> [s] 
                        -> Network l
                        -> GLO.Picture
renderFrameNetwork ar winSize@(wx, wy) ss e = GLO.Pictures [envPics, agentPics]
    where
        agentPics = GLO.Blank
        envPics = GLO.Blank

defaultAgentRendererNetwork :: AgentColorerNetwork s -> AgentRendererNetwork s l
defaultAgentRendererNetwork acf (x, y) size state link = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 size
    where
        color = acf state

defaultAgentColorerNetwork :: GLO.Color -> AgentColorerNetwork s
defaultAgentColorerNetwork color _ = color