module SugarScape.SugarScapeRenderer where

import FrABS.Agent.Agent
import SugarScape.SugarScapeModel
import FrABS.Env.Environment

import qualified Graphics.Gloss as GLO

display :: String -> (Int, Int) -> GLO.Display
display title winSize = (GLO.InWindow title winSize (0, 0))

renderFrame :: [SugarScapeAgentOut] -> SugarScapeEnvironment -> (Int, Int) -> GLO.Picture
renderFrame aouts env wSize@(wx, wy) = GLO.Pictures $ (envPics ++ agentPics)
    where
        (cx, cy) = envLimits env
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)

        cells = allCellsWithCoords env

        agentPics = map (renderAgent (cellWidth, cellHeight) wSize) aouts
        envPics = map (renderEnvCell (cellWidth, cellHeight) wSize) cells

renderEnvCell :: (Float, Float) -> (Int, Int) -> (EnvCoord, SugarScapeEnvCell) -> GLO.Picture
renderEnvCell (rectWidth, rectHeight) (wx, wy) ((x, y), cell) = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle 0 radius
    where
        color = GLO.makeColor (realToFrac 0.9) (realToFrac 0.9) (realToFrac 0.0) 1.0

        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize

        sugarLevel = sugEnvSugarLevel cell
        sugarLevelRatio = sugarLevel / (snd sugarCapacityRange)

        radius = rectWidth * (realToFrac sugarLevelRatio)

renderAgent :: (Float, Float) -> (Int, Int) -> SugarScapeAgentOut -> GLO.Picture
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
agentColor a = GLO.makeColor (realToFrac 0.7) (realToFrac 0.0) (realToFrac 0.0) 1.0