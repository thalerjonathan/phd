module SugarScape.SugarScapeRenderer where

import FrABS.Agent.Agent
import SugarScape.SugarScapeModel
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

import Debug.Trace

display :: String -> (Int, Int) -> GLO.Display
display title winSize = (GLO.InWindow title winSize (0, 0))

renderFrame :: [SugarScapeAgentOut] -> SugarScapeEnvironment -> (Int, Int) -> GLO.Picture
renderFrame aouts env wSize@(wx, wy) = GLO.Pictures $ (envPics ++ agentPics)
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
        envPics = trace ("maxpol=" ++ (show maxPolLevel)) (map (renderEnvCell (cellWidth, cellHeight) wSize maxPolLevel) cells)

renderEnvCell :: (Float, Float)
                    -> (Int, Int)
                    -> Double
                    -> (EnvCoord, SugarScapeEnvCell)
                    -> GLO.Picture
renderEnvCell (rectWidth, rectHeight) (wx, wy) maxPolLevel ((x, y), cell) = GLO.Pictures $ [polutionLevelRect, sugarLevelCircle]
    where
        polLevel = sugEnvPolutionLevel cell
        polGreenShadeRelative = (realToFrac (polLevel / maxPolLevel))
        polGreenShadeAbsolute = 1.0 - (min 1.0 (realToFrac (polLevel / 30)))
        polGreenShade = polGreenShadeAbsolute

        sugarColor = GLO.makeColor (realToFrac 0.9) (realToFrac 0.9) (realToFrac 0.0) 1.0
        polutionColor = if polLevel == 0.0 then GLO.white else GLO.makeColor (realToFrac 0.0) polGreenShade (realToFrac 0.0) 1.0

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        sugarLevel = sugEnvSugarLevel cell
        sugarLevelRatio = sugarLevel / (snd sugarCapacityRange)

        radius = rectWidth * (realToFrac sugarLevelRatio)
        sugarLevelCircle = GLO.color sugarColor $ GLO.translate xPix yPix $ GLO.ThickCircle 0 radius
        polutionLevelRect = GLO.color polutionColor $ GLO.translate xPix yPix $ GLO.rectangleSolid rectWidth rectHeight

renderAgent :: (Float, Float)
                -> (Int, Int)
                -> SugarScapeAgentOut
                -> GLO.Picture
renderAgent (rectWidth, rectHeight) (wx, wy) a = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 rectWidth
    where
        s = aoState a
        (x, y) = aoEnvPos a
        color = agentColor a

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

agentColor :: SugarScapeAgentOut -> GLO.Color
agentColor a
    | isMale = GLO.makeColor (realToFrac 1.0) (realToFrac 0.1) (realToFrac 0.1) 1.0
    | isFemale = GLO.makeColor (realToFrac 0.0) (realToFrac 0.3) (realToFrac 0.6) 1.0
    where
        isMale = Male == (sugAgGender $ aoState a)
        isFemale = not isMale