module Main where

import qualified Graphics.Gloss as GLO

main :: IO ()
main = GLO.display (GLO.InWindow "Haskell Art: Functional Islamic Design" (800, 800) (10, 10)) GLO.black (GLO.color GLO.white constructPicture)

constructPicture :: GLO.Picture
constructPicture = GLO.Pictures pic
    where
        r = 80
        x = r / 2
        y = sqrt((3 * r * r) / 4)
        line1 = drawCircleLine 0 0 r
        line2 = drawCircleLine x y r
        line3 = drawCircleLine 0 (2*y) r
        pic = line1 ++ line2 ++ line3

drawCircleLine :: Float -> Float -> Float -> [GLO.Picture]
drawCircleLine x y r  = map (\i -> drawCircleAt (x + (r * i)) y r) [-3..3]

constructPicture'' :: GLO.Picture
constructPicture'' = GLO.Pictures [center, c1, c2, c3]
    where
        r = 80
        x = r / 2
        y = sqrt( (3 * r * r)/4)
        center = drawCircleAt 0 0 2
        c1 = drawCircleAt 0 0 r
        c2 = drawCircleAt r 0 r
        c3 = drawCircleAt x (-y) r

constructPicture' :: GLO.Picture
constructPicture' = GLO.Pictures [center, c1, c2, c3, c4, c5, c6, c7]
    where
        r = 80
        rHalf = r / 2
        center = drawCircleAt 0 0 2
        c1 = drawCircleAt 0 0 r
        c2 = drawCircleAt r 0 r
        c3 = drawCircleAt rHalf (-r) r
        c4 = drawCircleAt (r + rHalf) (-r) r
        c5 = drawCircleAt (2*r) 0 r
        c6 = drawCircleAt rHalf (r) r
        c7 = drawCircleAt (r + rHalf) (r) r

drawCircleAt :: Float -> Float -> Float -> GLO.Picture
drawCircleAt x y r = GLO.translate x y $ GLO.Circle r