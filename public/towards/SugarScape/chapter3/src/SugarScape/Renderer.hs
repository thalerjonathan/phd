module SugarScape.Renderer 
  ( AgentObservable
  , AgentVis (..)
  , SiteVis (..)

  , renderSugarScapeFrame
  ) where

import Graphics.Gloss as GLO

import SugarScape.Agent.Interface
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Simulation

data AgentVis = Default 
              | Gender deriving (Eq, Show)

data SiteVis = Sugar 
             | Polution deriving (Eq, Show)

type SugEnvironmentRenderer = EnvRendererDisc2d SugEnvSite
type SugarScapeAgentRenderer = AgentRendererDisc2d SugAgentObservable

polBlackCap :: Double
polBlackCap = 50

renderSugarScapeFrame :: (Int, Int) 
                      -> Time 
                      -> Int
                      -> SugEnvironment
                      -> [AgentObservable SugAgentObservable]
                      -> AgentVis
                      -> SiteVis
                      -> GLO.Picture
renderSugarScapeFrame wSize@(wx, wy) t steps e ss av cv
    = GLO.Pictures (envPics ++ agentPics ++ [timeTxt, stepsTxt, asCntTxt, maxIdTxt])
  where
    (dx, dy)   = dimensionsDisc2d e
    siteWidth  = fromIntegral wx / fromIntegral dx
    siteHeight = fromIntegral wy / fromIntegral dy

    sites = allCellsWithCoords e

    agentPics = map (sugarscapeAgentRenderer av (siteWidth, siteHeight) wSize t) ss
    envPics   = map (renderEnvSite cv (siteWidth, siteHeight) wSize t) sites

    maxId = if null ss then 0 else maximum $ map fst ss

    timeTxt  = GLO.color GLO.black $ GLO.translate (-halfWSizeX) halfWSizeY $ GLO.scale 0.1 0.1 $ GLO.Text ("t = " ++ show t)
    stepsTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 20) $ GLO.scale 0.1 0.1 $ GLO.Text ("event count = " ++ show steps)
    asCntTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 40) $ GLO.scale 0.1 0.1 $ GLO.Text ("number of agents = " ++ show (length ss))
    maxIdTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 60) $ GLO.scale 0.1 0.1 $ GLO.Text ("total agents created = " ++ show maxId)

    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

renderEnvSite :: SiteVis -> SugEnvironmentRenderer
renderEnvSite Sugar r@(rw, _rh) w _t (coord, site) = sugLvlCircle
  where
    sugarColor   = GLO.makeColor 0.9 0.9 0.0 1.0
    (x, y)       = transformToWindow r w coord
    sugLvl       = sugEnvSiteSugarLevel site
    sugRatio     = (sugLvl / fromIntegral maxSugarCapacitySite) :: Double
    sugRadius    = rw * realToFrac sugRatio
    sugLvlCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugRadius

renderEnvSite Polution r@(rw, rh) w _t (coord, site) = polLvlSquare
  where
    (x, y)       = transformToWindow r w coord
    polLvl       = sugEnvSitePolutionLevel site
    polRatio     = 1.0 - min 1.0 (realToFrac (polLvl / polBlackCap)) 
    polColor     = if polLvl == 0 then GLO.white else GLO.makeColor 0 polRatio 0 1
    polLvlSquare = GLO.color polColor $ GLO.translate x y $ GLO.rectangleSolid rw rh

sugarscapeAgentRenderer :: AgentVis -> SugarScapeAgentRenderer
sugarscapeAgentRenderer av r@(rw, rh) w _t (aid, s) 
    = GLO.Pictures [circ, txt]
  where
    coord = sugObsCoord s
    col   = agentColor av
              
    (x, y) = transformToWindow r w coord 

    circ   = GLO.color col $ GLO.translate x y $ GLO.ThickCircle 0 rw
    txt    = GLO.color GLO.white $ GLO.translate (x - (rw * 0.4)) (y - (rh * 0.1)) $ GLO.scale 0.04 0.04 $ GLO.Text (show aid)

    agentColor :: AgentVis -> GLO.Color
    agentColor Gender = genderColor (sugObsGender s)
    agentColor _      = GLO.blue
    
    genderColor :: AgentGender -> GLO.Color
    genderColor Male   = GLO.blue
    genderColor Female = GLO.rose

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