module SocialForce.Renderer (
    renderSocialForceFrame
  ) where

import SocialForce.Model
import SocialForce.Markup

import FRP.FrABS
import Text.Printf

import qualified Graphics.Gloss as GLO

type SocialForceRenderFrame = RenderFrame SocialForceAgentState SocialForceEnvironment

renderSocialForceFrame :: SocialForceRenderFrame
renderSocialForceFrame wSize@(wx, wy) t ss e = 
    flipPic $ translatePic $ GLO.Pictures $ timeStepPic : envPic : agentPics
  where
    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

    translatePic = GLO.translate (-halfWSizeX) (-halfWSizeY)
    flipPic = GLO.scale (1.0) (-1.0)

    timeStepPic = GLO.color GLO.white $ GLO.translate 15 15 $ GLO.scale 0.1 (-0.1) $ GLO.Text timeTxt

    envPic = renderSocialForceEnvironment e
    agentPics = map renderSocialForceAgents ss

    timeTxt = printf "%.2f" t

renderSocialForceAgents :: SocialForceAgentObservable -> GLO.Picture 
renderSocialForceAgents (aid, s) = GLO.Blank -- TODO: implement
  
renderSocialForceEnvironment :: SocialForceEnvironment -> GLO.Picture 
renderSocialForceEnvironment e = GLO.Pictures wallPics
  where
    wallPics = map renderWall (sfEnvWalls e)

renderWall :: Wall -> GLO.Picture 
renderWall w = GLO.color GLO.white $ GLO.Line ps
  where
    ps = map (\(x, y) -> (realToFrac x, realToFrac y)) w