module WildFireFrontend where

import WildfireModel

import qualified Graphics.Gloss as GLO

winSizeX :: Int
winSizeX = 500

winSizeY :: Int
winSizeY = 500

display :: GLO.Display
display = (GLO.InWindow "Wildfire (Gloss)" (winSizeX, winSizeY) (0, 0))

renderFrame :: [(Int, Int, WFState, Double)] -> (Int, Int) -> GLO.Picture
renderFrame as (xCells, yCells) = GLO.Pictures $ agentPics
    where
        agentPics = map (renderAgent (cellWidth, cellHeight)) as
        cellWidth = (fromIntegral winSizeX) / (fromIntegral xCells)
        cellHeight = (fromIntegral winSizeY) / (fromIntegral yCells)

renderAgent :: (Float, Float) -> (Int, Int, WFState, Double) -> GLO.Picture
renderAgent (rectWidth, rectHeight) (x, y, s, b) = GLO.color color $ GLO.translate xPix yPix $ GLO.Polygon (GLO.rectanglePath rectWidth rectHeight)
    where
        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        color = agentColor s b
        halfXSize = fromRational (toRational winSizeX / 2.0)
        halfYSize = fromRational (toRational winSizeY / 2.0)

agentColor :: WFState -> Double -> GLO.Color
agentColor Living b = GLO.makeColor 0.0 (realToFrac b) 0.0 1.0
agentColor Burning b = GLO.makeColor (realToFrac b) 0.0 0.0 1.0
agentColor Dead b = GLO.black
