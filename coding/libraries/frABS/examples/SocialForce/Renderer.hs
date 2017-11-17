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
    flipPic $ translatePic $ scaleToPixPic $ GLO.Pictures $ timeStepPic : envPic : agentPics
  where
    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

    translatePic = GLO.translate (-halfWSizeX) (-halfWSizeY)
    scaleToPixPic = GLO.scale (25.0) (25.0)
    flipPic = GLO.scale (1.0) (-1.0)

    timeStepPic = GLO.color GLO.white $ GLO.translate 15 15 $ GLO.scale 0.1 (-0.1) $ GLO.Text timeTxt

    envPic = renderSocialForceEnvironment e
    agentPics = map renderSocialForceAgents ss

    timeTxt = printf "%.2f" t

renderSocialForceAgents :: SocialForceAgentObservable -> GLO.Picture 
renderSocialForceAgents (aid, s) 
  | isPerson s = renderPerson s
  | otherwise = GLO.Blank

renderPerson :: SocialForceAgentState -> GLO.Picture 
renderPerson (Person { perPos = (x, y) }) = GLO.color GLO.white $ GLO.translate x' y' $ GLO.ThickCircle 0 0.1
  where
    x' = realToFrac x
    y' = realToFrac y

renderSocialForceEnvironment :: SocialForceEnvironment -> GLO.Picture 
renderSocialForceEnvironment e = GLO.Pictures wallPics
  where
    wallPics = map renderLine (sfEnvWalls e)

renderLine :: Line -> GLO.Picture 
renderLine (from, to) = GLO.color GLO.white $ GLO.Line [(fx, fy), (tx, ty)]
  where
    fx = realToFrac $ fst from
    fy = realToFrac $ snd from

    tx = realToFrac $ fst to
    ty = realToFrac $ snd to