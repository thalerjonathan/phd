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
renderZombiesFrame wSize@(wx, wy) t ss (as, ap, an) = agentsPic -- GLO.Pictures [patchesPic, agentsPic]
    where
        {-
        patchesPic = renderFrameDisc2d 
                        voidAgentRendererDisc2d
                        voidEnvRendererDisc2d -- zombiesEnvRenderer -- voidEnvRendererDisc2d
                        wSize
                        []
                        ap
        -}

        agentsPic = renderFrameCont2d 
                        (defaultAgentRendererCont2d agentSize zombiesAgentColorer zAgentCoord)
                        voidEnvRendererCont2d
                        wSize
                        t
                        ss
                        as

zombiesEnvRenderer :: ZombiesEnvRenderer
zombiesEnvRenderer r@(rw, rh) w _t (coord, cell) = GLO.color GLO.white $ GLO.translate x y $ GLO.scale 0.1 0.1 $ GLO.Text (show (zombieCount, humans))
    where
        (x, y) = transformToWindow r w coord
        zombieCount = snd cell
        humanCount = humansOnPatch cell
        humans = fst cell

zombiesAgentColorer :: ZombiesAgentColorer 
zombiesAgentColorer s 
    | isHuman s = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    | otherwise = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0