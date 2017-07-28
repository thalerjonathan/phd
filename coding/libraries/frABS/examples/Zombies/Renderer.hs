module Zombies.Renderer (
    renderZombiesFrame
  ) where

import           FRP.FrABS

import           Zombies.Model

import qualified Graphics.Gloss  as GLO

type ZombiesRenderFrame = RenderFrame ZombiesAgentState ZombiesEnvironment 
type ZombiesEnvRenderer = EnvRendererDisc2d ZombiesPatch
type ZombiesAgentColorer = AgentColorerCont2d ZombiesAgentState

agentSize = 15
agentColor = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

renderZombiesFrame :: ZombiesRenderFrame
renderZombiesFrame wSize@(wx, wy) ss e = GLO.Pictures [patchesPic, agentsPic]
    where
        ap = zAgentPatches e
        as = zAgentSpace e

        patchesPic = renderFrameDisc2d 
                        voidAgentRendererDisc2d --(defaultAgentRendererDisc2d (defaultAgentColorerDisc2d agentColor) agentZeroDiscCoord) -- voidAgentRendererDisc2d
                        zombiesEnvRenderer
                        wSize
                        [] -- ss
                        ap

        agentsPic = renderFrameCont2d 
                        (defaultAgentRendererCont2d agentSize zombiesAgentColorer zAgentCoord)
                        voidEnvRendererCont2d
                        wSize
                        ss
                        as

zombiesEnvRenderer :: ZombiesEnvRenderer
zombiesEnvRenderer r@(rw, rh) w (coord, cell) = GLO.color GLO.white $ GLO.translate x y $ GLO.scale 0.05 0.05 $ GLO.Text (show cell)
    where
        (x, y) = transformToWindow r w coord

zombiesAgentColorer :: ZombiesAgentColorer 
zombiesAgentColorer s 
    | isHuman s = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    | otherwise = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0