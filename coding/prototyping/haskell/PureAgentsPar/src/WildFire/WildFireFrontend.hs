module WildFire.WildFireFrontend where

import qualified Graphics.Gloss as GLO

winSizeX :: Int
winSizeX = 800

winSizeY :: Int
winSizeY = 800

display :: GLO.Display
display = (GLO.InWindow "Wildfire Dynamic (Gloss)" (winSizeX, winSizeY) (0, 0))

data RenderCellState = ShadeGreen | ShadeRed | ShadeGray

data RenderCell = RenderCell {
    renderCellCoord :: (Int, Int),
    renderCellShade :: Double,
    renderCellState :: RenderCellState
}

renderFrame :: [RenderCell] -> (Int, Int) -> GLO.Picture
renderFrame cs (xCells, yCells) = GLO.Pictures $ agentPics
    where
        agentPics = map (renderCell (cellWidth, cellHeight)) cs
        cellWidth = (fromIntegral winSizeX) / (fromIntegral xCells)
        cellHeight = (fromIntegral winSizeY) / (fromIntegral yCells)

renderCell :: (Float, Float) -> RenderCell -> GLO.Picture
renderCell (rectWidth, rectHeight) c = GLO.color color $ GLO.translate xPix yPix $ GLO.Polygon (GLO.rectanglePath rectWidth rectHeight)
    where
        (x, y) = renderCellCoord c
        s = renderCellState c
        b = renderCellShade c
        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        color = cellColor s b
        halfXSize = fromRational (toRational winSizeX / 2.0)
        halfYSize = fromRational (toRational winSizeY / 2.0)

cellColor :: RenderCellState -> Double -> GLO.Color
cellColor ShadeGreen b = GLO.makeColor 0.0 (realToFrac b) 0.0 1.0
cellColor ShadeRed b = GLO.makeColor (realToFrac b) 0.0 0.0 1.0
cellColor ShadeGray b = GLO.greyN 0.5
