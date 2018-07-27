module Renderer 
  (
    AgentObservable

  , renderSugarScapeFrame
  ) where

import           FRP.BearRiver
import qualified Graphics.Gloss as GLO

import           Discrete
import           Model

type SugarScapeAgentColorer = AgentCellColorerDisc2d SugAgentObservable
type SugEnvironmentRenderer = EnvRendererDisc2d SugEnvCell
type SugarScapeAgentRenderer = AgentRendererDisc2d SugAgentObservable

renderSugarScapeFrame :: (Int, Int) 
                      -> Time 
                      -> Discrete2dDimension
                      -> [Discrete2dCell SugEnvCell]
                      -> [AgentObservable SugAgentObservable]
                      -> GLO.Picture
renderSugarScapeFrame wSize@(wx, wy) t (dx, dy) cs ss
    = GLO.Pictures (envPics ++ agentPics ++ [timeStepTxt])
  where
    cellWidth = fromIntegral wx / fromIntegral dx
    cellHeight = fromIntegral wy / fromIntegral dy

    agentPics = map (sugarscapeAgentRenderer (cellWidth, cellHeight) wSize t) ss
    envPics = map (renderEnvCell (cellWidth, cellHeight) wSize t) cs

    timeStepTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY - 20) $ GLO.scale 0.1 0.1 $ GLO.Text (show t)

    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

renderEnvCell :: SugEnvironmentRenderer
renderEnvCell r@(rw, _rh) w _t (coord, cell)
    | sugarRatio <= 0.01 = GLO.blank
    | otherwise          = sugarLevelCircle
  where
    sugarColor = GLO.makeColor 0.9 0.9 0.0 1.0

    (x, y) = transformToWindow r w coord

    sugarLevel = sugEnvSugarLevel cell
    sugarRatio = sugarLevel / snd sugarCapacityRange

    sugarRadius = rw * realToFrac sugarRatio
    sugarLevelCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugarRadius

sugarscapeAgentRenderer :: SugarScapeAgentRenderer
sugarscapeAgentRenderer r@(rw, rh) w _t (aid, s) 
    = GLO.Pictures [circle, txt]
  where
    coord = sugObsCoord s
    color = GLO.blue
    (x, y) = transformToWindow r w coord 

    circle = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 rw
    txt = GLO.color GLO.white $ GLO.translate (x - (rw * 0.4)) (y - (rh * 0.1)) $ GLO.scale 0.04 0.04 $ GLO.Text (show aid)

-------------------------------------------------------------------------------
type AgentObservable o   = (AgentId, o)
                            
type AgentRendererDisc2d s = (Float, Float) 
                           -> (Int, Int) 
                           -> Time
                           -> (AgentId, s)
                           -> GLO.Picture
type AgentCellColorerDisc2d s = s -> GLO.Color
type AgentCoordDisc2d s = (s -> Discrete2dCoord)


type EnvRendererDisc2d c        = (Float, Float) 
                                -> (Int, Int)
                                -> Time 
                                -> (Discrete2dCoord, c) 
                                -> GLO.Picture

_defaultAgentRendererDisc2d :: AgentCellColorerDisc2d s 
                              -> AgentCoordDisc2d s 
                              -> AgentRendererDisc2d s
_defaultAgentRendererDisc2d acf apf r@(rw, _) w _t (_, s) = 
    GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 rw
  where
    coord = apf s
    color = acf s
    (x, y) = transformToWindow r w coord

defaultAgentColorerDisc2d :: GLO.Color -> AgentCellColorerDisc2d s
defaultAgentColorerDisc2d color _ = color

transformToWindow :: (Float, Float)
                      -> (Int, Int) 
                      -> Discrete2dCoord 
                      -> (Float, Float)
transformToWindow (rw, rh) (wx, wy) (x, y) = (x', y')
  where
    halfXSize = fromRational (toRational wx / 2.0)
    halfYSize = fromRational (toRational wy / 2.0)

    x' = fromRational (toRational (fromIntegral x * rw)) - halfXSize
    y' = fromRational (toRational (fromIntegral y * rh)) - halfYSize