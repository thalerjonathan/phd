module HACFrontend where

import Data.IORef
import qualified Graphics.Gloss as GLO

winSizeX :: Int
winSizeX = 500

winSizeY :: Int
winSizeY = 500

agentSize :: Float
agentSize = 1

display :: GLO.Display
display = (GLO.InWindow "Heroes & Cowards (Gloss)" (winSizeX, winSizeY) (0, 0))

renderFrame :: [(Double, Double, Bool)] -> GLO.Picture
renderFrame as = GLO.Pictures $ agentPics
    where
        agentPics = map renderAgent as

renderAgent :: (Double, Double, Bool) -> GLO.Picture
renderAgent (x, y, hero) = GLO.color color $ GLO.translate xPix yPix $ GLO.ThickCircle agentSize (2*agentSize)
    where
        xPix = fromRational (toRational (x * fromRational halfXSize))
        yPix = fromRational (toRational (y * fromRational halfYSize))
        color = agentColor hero
        halfXSize = toRational winSizeX / 2.0
        halfYSize = toRational winSizeY / 2.0

agentColor :: Bool -> GLO.Color
agentColor hero
    | hero == True = GLO.green
    | hero == False = GLO.red
