module SugarScape.SugarScapeRenderer (
    renderFrame
  ) where

import FrABS.Agent.Agent
import SugarScape.SugarScapeModel
import SugarScape.SugarScapeAgentCommon
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

renderFrame :: (Int, Int) -> [SugarScapeAgentOut] -> SugarScapeEnvironment -> GLO.Picture
renderFrame wSize@(wx, wy) aouts env = GLO.Pictures $ (envPics ++ agentPics)
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        maxPolLevel = foldr (\(_, cell) maxLvl -> if sugEnvPolutionLevel cell > maxLvl then
                                                    sugEnvPolutionLevel cell
                                                    else
                                                        maxLvl ) 0.0 cells

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts
        envPics = (map (renderEnvCell (cellWidth, cellHeight) wSize maxPolLevel) cells)

renderEnvCell :: (Float, Float)
                    -> (Int, Int)
                    -> Double
                    -> (EnvCoord, SugarScapeEnvCell)
                    -> GLO.Picture
renderEnvCell (rectWidth, rectHeight) (wx, wy) maxPolLevel ((x, y), cell) = GLO.Pictures $ [polutionLevelRect, spiceLevelCircle, sugarLevelCircle]
    where
        polLevel = sugEnvPolutionLevel cell
        --polGreenShadeRelative = (realToFrac (polLevel / maxPolLevel))
        polGreenShadeAbsolute = 1.0 - (min 1.0 (realToFrac (polLevel / 30)))
        polGreenShade = polGreenShadeAbsolute

        sugarColor = GLO.makeColor (realToFrac 0.9) (realToFrac 0.9) (realToFrac 0.0) 1.0
        spiceColor = GLO.makeColor (realToFrac 0.9) (realToFrac 0.7) (realToFrac 0.0) 1.0
        polutionColor = if polLevel == 0.0 then GLO.white else GLO.makeColor (realToFrac 0.0) polGreenShade (realToFrac 0.0) 1.0

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        sugarLevel = sugEnvSugarLevel cell
        sugarRatio = sugarLevel / (snd sugarCapacityRange)

        spiceLevel = sugEnvSpiceLevel cell
        spiceRatio = spiceLevel / (snd spiceCapacityRange)

        sugarRadius = rectWidth * (realToFrac sugarRatio)
        sugarLevelCircle = GLO.color sugarColor $ GLO.translate xPix yPix $ GLO.ThickCircle 0 sugarRadius

        spiceRadius = rectWidth * (realToFrac spiceRatio)
        spiceLevelCircle = GLO.color spiceColor $ GLO.translate xPix yPix $ GLO.ThickCircle 0 spiceRadius

        polutionLevelRect = GLO.color polutionColor $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> SugarScapeAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        (x, y) = aoEnvPos a
        color = agentColorDiseased a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

agentColorDiseased :: SugarScapeAgentOut -> GLO.Color
agentColorDiseased a
    | isDiseased s = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        s = aoState a

agentColorGender :: SugarScapeAgentOut -> GLO.Color
agentColorGender a
    | isMale = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        isMale = Male == (sugAgGender $ aoState a)

agentColorTribe:: SugarScapeAgentOut -> GLO.Color
agentColorTribe a
    | tribe == Red = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | otherwise = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        tribe = sugAgTribe $ aoState a