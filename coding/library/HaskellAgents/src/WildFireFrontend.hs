module WildFireFrontend where

import WildfireModelDynamic

import qualified Graphics.Gloss as GLO

winSizeX :: Int
winSizeX = 500

winSizeY :: Int
winSizeY = 500

display :: GLO.Display
display = (GLO.InWindow "Wildfire Dynamic (Gloss)" (winSizeX, winSizeY) (0, 0))

renderFrame :: [WFCell] -> (Int, Int) -> GLO.Picture
renderFrame as (xCells, yCells) = GLO.Pictures $ agentPics
    where
        agentPics = map (renderCell (cellWidth, cellHeight)) as
        cellWidth = (fromIntegral winSizeX) / (fromIntegral xCells)
        cellHeight = (fromIntegral winSizeY) / (fromIntegral yCells)

renderCell :: (Float, Float) -> WFCell -> GLO.Picture
renderCell (rectWidth, rectHeight) c = GLO.color color $ GLO.translate xPix yPix $ GLO.Polygon (GLO.rectanglePath rectWidth rectHeight)
    where
        (x, y) = coord c
        s = cellState c
        b = burnable c
        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        color = cellColor s b
        halfXSize = fromRational (toRational winSizeX / 2.0)
        halfYSize = fromRational (toRational winSizeY / 2.0)

cellColor :: WFCellState -> Double -> GLO.Color
cellColor Living b = GLO.makeColor 0.0 (realToFrac b) 0.0 1.0
cellColor Burning b = GLO.makeColor (realToFrac b) 0.0 0.0 1.0
cellColor Dead b = GLO.greyN 0.5
