module SugarScape.Visual.Renderer 
  ( AgentObservable
  , AgentColoring (..)
  , SiteColoring (..)

  , renderSugarScapeFrame
  ) where

import Data.Maybe
import System.IO.Unsafe

import Control.Monad.STM
import Graphics.Gloss as GLO

import SugarScape.Agent.Common
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Simulation

data AgentColoring = Default 
                   | Gender 
                   | Culture
                   | Tribe
                   | Welfare
                   | Disease
                   deriving (Eq, Show, Read)

data SiteColoring = Resource
                  | Polution 
                  deriving (Eq, Show, Read)

type SugEnvironmentRenderer = EnvRendererDisc2d SugEnvSite
type SugarScapeAgentRenderer = AgentRendererDisc2d SugAgentObservable

polBlackCap :: Double
polBlackCap = 50

renderSugarScapeFrame :: (Int, Int) 
                      -> Time 
                      -> SugEnvironment
                      -> [AgentObservable SugAgentObservable]
                      -> AgentColoring
                      -> SiteColoring
                      -> GLO.Picture
renderSugarScapeFrame wSize@(wx, wy) t e ss av cv
    = GLO.Pictures (envPics ++ agentPics ++ [timeTxt, asCntTxt, maxIdTxt])
  where
    -- TODO: provide a safe implementation
    (dx, dy)   = unsafePerformIO $ atomically $ dimensionsDisc2d e
    siteWidth  = fromIntegral wx / fromIntegral dx
    siteHeight = fromIntegral wy / fromIntegral dy

    -- TODO: provide a safe implementation
    sites = unsafePerformIO $ atomically $ allCellsWithCoords e

    agentPics = map (sugarscapeAgentRenderer av (siteWidth, siteHeight) wSize t) ss
    envPics   = mapMaybe (renderEnvSite cv (siteWidth, siteHeight) wSize t) sites

    maxId = if null ss then 0 else maximum $ map fst ss

    timeTxt  = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 20) $ GLO.scale 0.1 0.1 $ GLO.Text ("t = " ++ show t)
    asCntTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 40) $ GLO.scale 0.1 0.1 $ GLO.Text ("number of agents = " ++ show (length ss))
    maxIdTxt = GLO.color GLO.black $ GLO.translate (-halfWSizeX) (halfWSizeY + 60) $ GLO.scale 0.1 0.1 $ GLO.Text ("total agents created = " ++ show maxId)

    halfWSizeX = fromIntegral wx / 2.0 
    halfWSizeY = fromIntegral wy / 2.0 

renderEnvSite :: SiteColoring -> SugEnvironmentRenderer
renderEnvSite Resource r@(rw, _rh) w _t (coord, site) 
    | null pics' = Nothing
    | otherwise  = Just $ GLO.Pictures pics'
  where
    (x, y)       = transformToWindow r w coord

    sugarColor   = GLO.makeColor 0.9 0.9 0.0 1.0
    sugLvl       = sugEnvSiteSugarLevel site
    sugRatio     = (sugLvl / fromIntegral maxSugarCapacitySite) :: Double
    sugRadius    = rw * realToFrac sugRatio
    sugLvlCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugRadius

    spiceColor     = GLO.makeColor 0.9 0.7 0.0 1.0
    spiceLvl       = sugEnvSiteSpiceLevel site
    spiceRatio     = (spiceLvl / fromIntegral maxSpiceCapacitySite) :: Double
    spiceRadius    = rw * realToFrac spiceRatio
    spiceLvlCircle = GLO.color spiceColor $ GLO.translate x y $ GLO.ThickCircle 0 spiceRadius

    pics           = [sugLvlCircle | sugLvl > 0.1]
    pics'          = if spiceLvl > 0.1 then spiceLvlCircle : pics else pics

renderEnvSite Polution r@(rw, rh) w _t (coord, site) = Just polLvlSquare
  where
    (x, y)       = transformToWindow r w coord
    polLvl       = sugEnvSitePolutionLevel site
    polRatio     = 1.0 - min 1.0 (realToFrac (polLvl / polBlackCap)) 
    polColor     = if polLvl == 0 then GLO.white else GLO.makeColor 0 polRatio 0 1
    polLvlSquare = GLO.color polColor $ GLO.translate x y $ GLO.rectangleSolid rw rh

sugarscapeAgentRenderer :: AgentColoring -> SugarScapeAgentRenderer
sugarscapeAgentRenderer av r@(rw, rh) w _t (aid, s) 
    = GLO.Pictures [circ, txt]
  where
    coord = sugObsCoord s
    col   = agentColor av
              
    (x, y) = transformToWindow r w coord 

    circ   = GLO.color col $ GLO.translate x y $ GLO.ThickCircle 0 rw
    txt    = GLO.color GLO.white $ GLO.translate (x - (rw * 0.4)) (y - (rh * 0.1)) $ GLO.scale 0.04 0.04 $ GLO.Text (show aid)

    agentColor :: AgentColoring -> GLO.Color
    agentColor Gender  = genderColor (sugObsGender s)
    agentColor Culture = cultureColor (sugObsTribe s)
    agentColor Tribe   = cultureColor (sugObsTribe s)
    agentColor Welfare = welfareColor s
    agentColor Disease = diseaseColor (sugObsDiseases s)
    agentColor _       = mateBlue
    
    genderColor :: AgentGender -> GLO.Color
    genderColor Male   = mateBlue
    genderColor Female = GLO.rose

    cultureColor :: AgentTribe -> GLO.Color
    cultureColor Blue = mateBlue
    cultureColor Red  = mateRed

    welfareColor :: SugAgentObservable -> GLO.Color
    welfareColor obs 
        | m < 1     = mateBlue
        | otherwise = mateRed
      where
        m1  = fromIntegral $ sugObsSugMetab obs
        m2  = fromIntegral $ sugObsSpiMetab obs
        w1  = sugObsSugLvl obs
        w2  = sugObsSpiLvl obs
        m   = mrs w1 w2 m1 m2

    diseaseColor :: [Disease] -> GLO.Color
    diseaseColor ds 
      | null ds   = mateBlue
      | otherwise = mateRed

mateBlue :: GLO.Color
mateBlue = GLO.makeColor 0.0 0.0 0.9 1.0

mateRed :: GLO.Color
mateRed = GLO.makeColor 0.9 0.0 0.0 1.0


-------------------------------------------------------------------------------
type AgentRendererDisc2d s = (Float, Float) 
                           -> (Int, Int) 
                           -> Time
                           -> (AgentId, s)
                           -> GLO.Picture

type EnvRendererDisc2d c = (Float, Float) 
                         -> (Int, Int)
                         -> Time 
                         -> (Discrete2dCoord, c) 
                         -> Maybe GLO.Picture

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