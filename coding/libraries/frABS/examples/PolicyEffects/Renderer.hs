module PolicyEffects.Renderer (
    renderPolicyEffectsFrame
  ) where

import PolicyEffects.Model

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type PolicyEffectsRenderFrame = RenderFrame PolicyEffectsState PolicyEffectsMsg PolicyEffectsEnvCell PolicyEffectsEnvLink
type PolicyEffectsEnvCellColorer = EnvironmentCellColorer PolicyEffectsState

renderPolicyEffectsFrame :: PolicyEffectsRenderFrame
renderPolicyEffectsFrame wSize@(wx, wy) aouts env = GLO.Pictures $ agentPics
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        maxWealth = foldr (\ao m -> if (aoState ao > m) then aoState ao else m) 0 aouts
 
        agentColor = agentColorer maxWealth
        agentPics = map (defaultAgentRenderer agentColor (cellWidth, cellHeight) wSize) aouts

agentColorer :: Double -> PolicyEffectsEnvCellColorer
agentColorer maxWealth agentWealth = GLO.makeColor (realToFrac 0.0) (realToFrac 0.0) (realToFrac shade) 1.0 
    where
        shade = agentWealth / maxWealth