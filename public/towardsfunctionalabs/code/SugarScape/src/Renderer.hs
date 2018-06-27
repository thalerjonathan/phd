module Renderer 
  (
    AgentObservable

  , renderSugarScapeFrame
  ) where

import           FRP.BearRiver
import qualified Graphics.Gloss as GLO

import AgentMonad
import Discrete
import Model

type SugarScapeAgentColorer = AgentCellColorerDisc2d SugAgentObservable
type SugEnvironmentRenderer = EnvRendererDisc2d SugEnvCell
type SugarScapeAgentRenderer = AgentRendererDisc2d SugAgentObservable

renderSugarScapeFrame :: (Int, Int) 
                      -> Time 
                      -> SugEnvironment
                      -> [AgentObservable SugAgentObservable]
                      -> GLO.Picture
renderSugarScapeFrame wSize@(wx, wy) t e ss
    = GLO.Pictures (envPics ++ agentPics ++ [timeStepTxt])
  where
    (dx, dy) = dimensionsDisc2d e
    cellWidth = fromIntegral wx / fromIntegral dx
    cellHeight = fromIntegral wy / fromIntegral dy

    cells = allCellsWithCoords e

    maxPolLevel = foldr (\(_, cell) maxLvl -> if sugEnvPolutionLevel cell > maxLvl then
                                                sugEnvPolutionLevel cell
                                                else
                                                    maxLvl ) 0.0 cells

    -- agentPics = map (defaultAgentRendererDisc2d agentColorDiseased sugAgCoord (cellWidth, cellHeight) wSize) ss
    agentPics = map (sugarscapeAgentRenderer (cellWidth, cellHeight) wSize t) ss
    envPics = map (renderEnvCell maxPolLevel (cellWidth, cellHeight) wSize t) cells

    timeStepTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY - 20) $ GLO.scale 0.1 0.1 $ GLO.Text (show t)

    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

renderEnvCell :: Double -> SugEnvironmentRenderer
renderEnvCell _maxPolLevel r@(rw, rh) w _t (coord, cell)
    = GLO.Pictures [polutionLevelRect, spiceLevelCircle, sugarLevelCircle]
  where
    polLevel = sugEnvPolutionLevel cell
    --polGreenShadeRelative = (realToFrac (polLevel / maxPolLevel))
    polGreenShadeAbsolute = 1.0 - min 1.0 (realToFrac (polLevel / 30))
    polGreenShade = polGreenShadeAbsolute

    sugarColor = GLO.makeColor 0.9 0.9 0.0 1.0
    spiceColor = GLO.makeColor 0.9 0.7 0.0 1.0
    polutionColor = if polLevel == 0.0 then GLO.white else GLO.makeColor 0.0 polGreenShade 0.0 1.0

    (x, y) = transformToWindow r w coord

    sugarLevel = sugEnvSugarLevel cell
    sugarRatio = sugarLevel / snd sugarCapacityRange

    spiceLevel = sugEnvSpiceLevel cell
    spiceRatio = spiceLevel / snd spiceCapacityRange

    sugarRadius = rw * realToFrac sugarRatio
    sugarLevelCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugarRadius

    spiceRadius = rw * realToFrac spiceRatio
    spiceLevelCircle = GLO.color spiceColor $ GLO.translate x y $ GLO.ThickCircle 0 spiceRadius

    polutionLevelRect = GLO.color polutionColor $ GLO.translate x y $ GLO.rectangleSolid rw rh

sugarscapeAgentRenderer :: SugarScapeAgentRenderer
sugarscapeAgentRenderer r@(rw, rh) w _t (aid, s) 
    = GLO.Pictures [circle, txt]
  where
    coord = sugObsCoord s
    color = agentColoring _agentColoring_ aid s

    (x, y) = transformToWindow r w coord 

    circle = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 rw
    txt = GLO.color GLO.white $ GLO.translate (x - (rw * 0.3)) (y - (rh * 0.1)) $ GLO.scale 0.05 0.05 $ GLO.Text (show aid)

agentColoring :: AgentColoring -> AgentId -> SugarScapeAgentColorer
agentColoring Undefined _    = defaultAgentColorerDisc2d GLO.blue
agentColoring Gender _       = agentColorGender
agentColoring Diseased _     = agentColorDiseased
agentColoring Tribe _        = agentColorTribe
agentColoring (IdGE x) aid   = agentColorId x aid
agentColoring (VisionGE x) _ = agentColorVision x

agentColorVision :: Int -> SugarScapeAgentColorer
agentColorVision x o
  | sugObsVision o >= x = GLO.red
  | otherwise           = GLO.blue

agentColorId :: AgentId -> AgentId -> SugarScapeAgentColorer
agentColorId x aid _
  | aid >= x  = GLO.red
  | otherwise = GLO.blue

agentColorDiseased :: SugarScapeAgentColorer
agentColorDiseased o
  | (not . null . sugObsDiseases) o = GLO.makeColor 1.0 0.1 0.1 1.0
  | otherwise    = GLO.makeColor 0.0 0.3 0.6 1.0

agentColorGender :: SugarScapeAgentColorer
agentColorGender o
    | isMale    = GLO.makeColor 1.0 0.1 0.1 1.0
    | otherwise = GLO.makeColor 0.0 0.3 0.6 1.0
  where
    isMale = Male == sugObsGender o

agentColorTribe:: SugarScapeAgentColorer
agentColorTribe o
    | tribe == Red = GLO.makeColor 1.0 0.1 0.1 1.0
    | otherwise    = GLO.makeColor 0.0 0.3 0.6 1.0
  where
    tribe = sugObsTribe o

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