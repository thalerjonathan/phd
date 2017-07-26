module SugarScape.Renderer (
    renderSugarScapeFrame
  ) where

import SugarScape.Model
import SugarScape.Common

import FRP.FrABS

import qualified Graphics.Gloss as GLO

type SugarScapeRenderFrame = RenderFrame SugarScapeAgentState SugarScapeEnvironment
type SugarScapeAgentColorer = AgentCellColorerDisc2d SugarScapeAgentState
type SugarScapeEnvironmentRenderer = EnvRendererDisc2d SugarScapeEnvCell
type SugarScapeAgentRenderer = AgentRendererDisc2d SugarScapeAgentState

renderSugarScapeFrame :: SugarScapeRenderFrame
renderSugarScapeFrame wSize@(wx, wy) ss e = GLO.Pictures $ (envPics ++ agentPics)
    where
        (dx, dy) = envDisc2dDims e
        cellWidth = fromIntegral wx / fromIntegral dx
        cellHeight = fromIntegral wy / fromIntegral dy

        cells = allCellsWithCoords e

        maxPolLevel = foldr (\(_, cell) maxLvl -> if sugEnvPolutionLevel cell > maxLvl then
                                                    sugEnvPolutionLevel cell
                                                    else
                                                        maxLvl ) 0.0 cells

        -- agentPics = map (defaultAgentRendererDisc2d agentColorDiseased sugAgCoord (cellWidth, cellHeight) wSize) ss
        agentPics = map (sugarscapeAgentRenderer (cellWidth, cellHeight) wSize) ss
        envPics = map (renderEnvCell maxPolLevel (cellWidth, cellHeight) wSize) cells

renderEnvCell :: Double -> SugarScapeEnvironmentRenderer
renderEnvCell maxPolLevel r@(rw, rh) w (coord, cell) = GLO.Pictures $ [polutionLevelRect, spiceLevelCircle, sugarLevelCircle]
    where
        polLevel = sugEnvPolutionLevel cell
        --polGreenShadeRelative = (realToFrac (polLevel / maxPolLevel))
        polGreenShadeAbsolute = 1.0 - (min 1.0 (realToFrac (polLevel / 30)))
        polGreenShade = polGreenShadeAbsolute

        sugarColor = GLO.makeColor (realToFrac 0.9) (realToFrac 0.9) (realToFrac 0.0) 1.0
        spiceColor = GLO.makeColor (realToFrac 0.9) (realToFrac 0.7) (realToFrac 0.0) 1.0
        polutionColor = if polLevel == 0.0 then GLO.white else GLO.makeColor (realToFrac 0.0) polGreenShade (realToFrac 0.0) 1.0

        (x, y) = transformToWindow r w coord

        sugarLevel = sugEnvSugarLevel cell
        sugarRatio = sugarLevel / (snd sugarCapacityRange)

        spiceLevel = sugEnvSpiceLevel cell
        spiceRatio = spiceLevel / (snd spiceCapacityRange)

        sugarRadius = rw * (realToFrac sugarRatio)
        sugarLevelCircle = GLO.color sugarColor $ GLO.translate x y $ GLO.ThickCircle 0 sugarRadius

        spiceRadius = rw * (realToFrac spiceRatio)
        spiceLevelCircle = GLO.color spiceColor $ GLO.translate x y $ GLO.ThickCircle 0 spiceRadius

        polutionLevelRect = GLO.color polutionColor $ GLO.translate x y $ GLO.rectangleSolid rw rh

sugarscapeAgentRenderer :: SugarScapeAgentRenderer
sugarscapeAgentRenderer r@(rw, rh) w (aid, s) = GLO.Pictures [circle, txt]
    where
        coord = sugAgCoord s
        color = agentColorDiseased s

        (x, y) = transformToWindow r w coord 

        circle = GLO.color color $ GLO.translate x y $ GLO.ThickCircle 0 rw
        txt = GLO.color GLO.white $ GLO.translate (x - (rw * 0.3)) (y - (rh * 0.1)) $ GLO.scale 0.05 0.05 $ GLO.Text (show aid)

agentColorDiseased :: SugarScapeAgentColorer
agentColorDiseased s
    | isDiseased s = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0

agentColorGender :: SugarScapeAgentColorer
agentColorGender s
    | isMale = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        isMale = Male == sugAgGender s

agentColorTribe:: SugarScapeAgentColorer
agentColorTribe s
    | tribe == Red = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        tribe = sugAgTribe s