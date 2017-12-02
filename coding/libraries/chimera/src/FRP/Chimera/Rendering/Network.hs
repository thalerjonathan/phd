module FRP.Chimera.Rendering.Network 
  (
    AgentRendererNetwork
  , AgentColorerNetwork

  , renderFrameNetwork

  , defaultAgentRendererNetwork
  , defaultAgentColorerNetwork
  ) where

import FRP.Yampa
import qualified Graphics.Gloss as GLO

import FRP.Chimera.Agent.Agent
import FRP.Chimera.Environment.Network
import FRP.Chimera.Rendering.GlossSimulator

-- TODO: implement graph-rendering

type AgentRendererNetwork s l   = (Float, Float)
                                -> Float
                                -> Time
                                -> (AgentId, s) 
                                -> l
                                -> GLO.Picture
type AgentColorerNetwork s      = s -> GLO.Color

renderFrameNetwork :: AgentRendererNetwork s l
                      -> RenderFrame s (Network l)
renderFrameNetwork _ar _winSize@(_wx, _wy) _t _ss _e = GLO.Pictures [envPics, agentPics]
  where
    agentPics = GLO.Blank
    envPics = GLO.Blank

defaultAgentRendererNetwork :: AgentColorerNetwork s -> AgentRendererNetwork s l
defaultAgentRendererNetwork acf (x, y) size _t (_, s) _link = 
    GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 size
  where
    color = acf s

defaultAgentColorerNetwork :: GLO.Color -> AgentColorerNetwork s
defaultAgentColorerNetwork color _ = color