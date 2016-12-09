module Main where

import qualified ARTRendering as Render
import qualified Graphics.Gloss as GLO

-- NOTE: order is x, y, r
data Circle = Circle Float Float Float

data Point = Point Float Float

main :: IO ()
main = render generateDescription

render :: [Circle] -> IO()
render cs = GLO.display
                (GLO.InWindow "Haskell Art: Functional Islamic Design" (800, 800) (10, 10))
                GLO.black
                (GLO.color GLO.white $ GLO.Pictures $ map toGlossPicture cs)

toGlossPicture :: Circle -> GLO.Picture
toGlossPicture (Circle x y r) = Render.drawCircleAt x y r

generateDescription :: [Circle]
generateDescription = [initC, c1, c2, c3, c4]
    where
        initC = Circle 0 0 80
        c1 = left initC
        c2 = right initC
        c3 = top initC
        c4 = bottom initC
        c5 = centerOfTwo c1 c2

left :: Circle -> Circle
left (Circle x y r) = Circle (x - r) y r

right :: Circle -> Circle
right (Circle x y r) = Circle (x + r) y r

top :: Circle -> Circle
top (Circle x y r) = Circle x (y + r) r

bottom :: Circle -> Circle
bottom (Circle x y r) = Circle x (y - r) r


centerOfTwo :: Circle -> Circle -> Circle
centerOfTwo (Circle x1 y1 r1) (Circle x2 y2 r2) = Circle xHalf y r1
    where
        xHalf = (x2 - x1) / 2
        y = sqrt((3 * r1 * r1) / 4)