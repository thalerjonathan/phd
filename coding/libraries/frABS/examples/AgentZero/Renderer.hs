module AgentZero.Renderer (
    renderAgentZeroFrame
  ) where

import           FRP.FrABS

import           AgentZero.Model

import qualified Graphics.Gloss  as GLO
 
type AgentZeroRenderFrame = RenderFrame AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell AgentZeroLink
type AgentZeroEnvCellColorer = EnvironmentCellColorer AgentZeroEnvCell

agentColor = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

renderAgentZeroFrame :: AgentZeroRenderFrame
renderAgentZeroFrame = render2dDiscreteFrame 
                            (defaultAgentRenderer (defaultAgentColorer agentColor))
                            (defaultEnvironmentRenderer agentZeroEnvCellColor)

agentZeroEnvCellColor :: AgentZeroEnvCellColorer
agentZeroEnvCellColor AgentZeroEnvCell {azCellState = state, azCellShade = shade}
    | Friendly == state = GLO.makeColor (realToFrac 1.0) (realToFrac 1.0) (realToFrac shade) 1.0
    | Attack == state = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | Dead == state = GLO.black