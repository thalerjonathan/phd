module SugarScape.Renderer 
  ( AgentObservable
  , CellVisualisation (..)

  , renderSugarScapeFrame
  ) where

import FRP.BearRiver
import Graphics.Gloss as GLO

import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Simulation

data CellVisualisation = Sugar | Polution deriving (Eq, Show)

type SugEnvironmentRenderer = EnvRendererDisc2d SugEnvCell
type SugarScapeAgentRenderer = AgentRendererDisc2d SugAgentObservable

polBlackCap :: Double
polBlackCap = 50

renderSugarScapeFrame :: (Int, Int) 
                      -> Time 
                      -> Int
                      -> SugEnvironment
                      -> [AgentObservable SugAgentObservable]
                      -> CellVisualisation
                      -> GLO.Picture
renderSugarScapeFrame wSize@(wx, wy) t steps e ss cv
    = GLO.Pictures (envPics ++ agentPics ++ [timeTxt, stepsTxt])
  where
    (dx, dy) = dimensionsDisc2d e
    cellWidth = fromIntegral wx / fromIntegral dx
    cellHeight = fromIntegral wy / fromIntegral dy

    cells = allCellsWithCoords e

    agentPics = map (sugarscapeAgentRenderer (cellWidth, cellHeight) wSize t) ss
    envPics = map (renderEnvCell cv (cellWidth, cellHeight) wSize t) cells

    timeTxt  = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY - 0) $ GLO.scale 0.1 0.1 $ GLO.Text (show t)
    stepsTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 20) $ GLO.scale 0.1 0.1 $ GLO.Text (show steps)

    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

renderEnvCell :: CellVisualisation -> SugEnvironmentRenderer
renderEnvCell Sugar r@(rw, rh) w _t (coord, cell) = sugLvlCircle
  where
    sugarColor   = GLO.makeColor 0.9 0.9 0.0 1.0
    (x, y)       = transformToWindow r w coord
    sugLvl       = sugEnvCellSugarLevel cell
    sugRatio     = (sugLvl / fromIntegral maxSugarCapacityCell) :: Double
    sugRadius    = rw * realToFrac sugRatio
    sugLvlCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugRadius

renderEnvCell Polution r@(rw, rh) w _t (coord, cell) = polLvlSquare
  where
    (x, y)       = transformToWindow r w coord
    polLvl       = sugEnvCellPolutionLevel cell
    polRatio     = 1.0 - min 1.0 (realToFrac (polLvl / polBlackCap)) 
    polColor     = if polLvl == 0 then GLO.white else GLO.makeColor 0 polRatio 0 1
    polLvlSquare = GLO.color polColor $ GLO.translate x y $ GLO.rectangleSolid rw rh

sugarscapeAgentRenderer :: SugarScapeAgentRenderer
sugarscapeAgentRenderer r@(rw, rh) w _t (aid, s) 
    = GLO.Pictures [circ, txt]
  where
    coord = sugObsCoord s
    col   = GLO.blue

    (x, y) = transformToWindow r w coord 

    circ   = GLO.color col $ GLO.translate x y $ GLO.ThickCircle 0 rw
    txt    = GLO.color GLO.white $ GLO.translate (x - (rw * 0.4)) (y - (rh * 0.1)) $ GLO.scale 0.04 0.04 $ GLO.Text (show aid)

-------------------------------------------------------------------------------
type AgentRendererDisc2d s = (Float, Float) 
                           -> (Int, Int) 
                           -> Time
                           -> (AgentId, s)
                           -> GLO.Picture

type EnvRendererDisc2d c        = (Float, Float) 
                                -> (Int, Int)
                                -> Time 
                                -> (Discrete2dCoord, c) 
                                -> GLO.Picture

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