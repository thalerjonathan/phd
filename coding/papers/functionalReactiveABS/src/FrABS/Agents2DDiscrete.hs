module FrABS.Agents2DDiscrete (
    RenderCellCoord,
    RenderCellColor,
    RenderCell(..),
    display,
    renderFrame
  ) where

import qualified Graphics.Gloss as GLO

type RenderCellCoord = (Int, Int)
type RenderCellColor = (Double, Double, Double)

data RenderCell = RenderCell {
    renderCellCoord :: RenderCellCoord,
    renderCellColor :: RenderCellColor
}

display :: String -> (Int, Int) -> GLO.Display
display title winSize = (GLO.InWindow title winSize (0, 0))

renderFrame :: [RenderCell] -> (Int, Int) -> (Int, Int) -> GLO.Picture
renderFrame cs wSize@(wx, wy) (cx, cy) = GLO.Pictures $ agentPics
    where
        agentPics = map (renderCell (cellWidth, cellHeight) wSize) cs
        cellWidth = (fromIntegral wx) / (fromIntegral cx)
        cellHeight = (fromIntegral wy) / (fromIntegral cy)



renderCell :: (Float, Float) -> (Int, Int) -> RenderCell -> GLO.Picture
renderCell (rectWidth, rectHeight) (wx, wy) c = GLO.color color $ GLO.translate xPix yPix $ GLO.Polygon (GLO.rectanglePath rectWidth rectHeight)
    where
        (x, y) = renderCellCoord c
        color = cellColor (renderCellColor c)
        xPix = fromRational (toRational (fromIntegral x * rectWidth)) - halfXSize
        yPix = fromRational (toRational (fromIntegral y * rectHeight)) - halfYSize
        halfXSize = fromRational (toRational wx / 2.0)
        halfYSize = fromRational (toRational wy / 2.0)

cellColor :: RenderCellColor -> GLO.Color
cellColor (r, g, b) = GLO.makeColor (realToFrac r) (realToFrac g) (realToFrac b) 1.0