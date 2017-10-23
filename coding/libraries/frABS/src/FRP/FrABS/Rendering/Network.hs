module FRP.FrABS.Rendering.Network (
    AgentRendererNetwork,
    AgentColorerNetwork,

    renderFrameNetwork,

    defaultAgentRendererNetwork,
    defaultAgentColorerNetwork
  ) where

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Network
import FRP.FrABS.Rendering.GlossSimulator

import qualified Graphics.Gloss as GLO

-- TODO: implement graph-rendering

type AgentRendererNetwork s l = (Float, Float)
                                -> Float
                                -> Time
                                -> (AgentId, s) 
                                -> l
                                -> GLO.Picture
type AgentColorerNetwork s = s -> GLO.Color

renderFrameNetwork :: AgentRendererNetwork s l
                        -> RenderFrame s (Network l)
renderFrameNetwork ar winSize@(wx, wy) t ss e = GLO.Pictures [envPics, agentPics]
    where
        agentPics = GLO.Blank
        envPics = GLO.Blank

defaultAgentRendererNetwork :: AgentColorerNetwork s -> AgentRendererNetwork s l
defaultAgentRendererNetwork acf (x, y) size t (_, s) link = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 size
    where
        color = acf s

defaultAgentColorerNetwork :: GLO.Color -> AgentColorerNetwork s
defaultAgentColorerNetwork color _ = color