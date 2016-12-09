module Main where

import Debug.Trace

import qualified ARTRendering as Render
import qualified Graphics.Gloss as GLO

-- NOTE: order is x, y, r
data Circle = Circle Float Float Float deriving (Show)

data Point = Point Float Float deriving (Show)

main :: IO ()
main = render generateDescription

render :: [Circle] -> IO()
render cs = GLO.display
                (GLO.InWindow "Haskell Art: Functional Islamic Design" (800, 800) (10, 10))
                GLO.black
                (GLO.color GLO.white $ GLO.Pictures $ map toGlossPicture cs)

toGlossPicture :: Circle -> GLO.Picture
toGlossPicture (Circle x y r) = Render.drawCircleAt x y r

circleFromPoint :: Float -> Point -> Circle
circleFromPoint r (Point x y) = Circle x y r

generateDescription :: [Circle]
generateDescription = [c1, c2, c3, c4, c5]
    where
        r = 80
        c1 = Circle 0 0 r
        c2 = right c1
        (c3, c4) = intersect c1 c2
        (c5, _) = intersect c1 c3
        (c6, _) = intersect c1 c4

left :: Circle -> Circle
left (Circle x y r) = Circle (x - r) y r

right :: Circle -> Circle
right (Circle x y r) = Circle (x + r) y r

top :: Circle -> Circle
top (Circle x y r) = Circle x (y + r) r

bottom :: Circle -> Circle
bottom (Circle x y r) = Circle x (y - r) r

-- NOTE: this assumes there are exactly 2 intersection-points and r0 == r1
intersect :: Circle -> Circle -> (Circle, Circle)
intersect c0@(Circle x0 y0 r0) c1@(Circle x1 y1 r1) = (ci1, ci2)
    where
        r = r0
        pis = intersectRaw c0 c1
        (p1, p2) = rawIntersectionToPair pis
        ci1 = circleFromPoint r p1
        ci2 = circleFromPoint r p2


rawIntersectionToPair :: Maybe [Point] -> (Point, Point)
rawIntersectionToPair Nothing = undefined
rawIntersectionToPair (Just (p1:p2:ps)) = (p1, p2)

intersectionsToCircles :: Maybe [Point] -> Float -> [Circle]
intersectionsToCircles Nothing _ = []
intersectionsToCircles (Just pis) r = map (circleFromPoint r) pis

-- NOTE: taken from http://paulbourke.net/geometry/circlesphere/tvoght.c
intersectRaw :: Circle -> Circle -> Maybe [Point]
intersectRaw (Circle x0 y0 r0) (Circle x1 y1 r1)
    | d == 0 = Nothing        -- OVERLAY, same center, infinite number solutions
    | d > (r0 + r1) = Nothing            -- SEPARATE, no solutions
    | d < abs (r0 - r1) = Nothing        -- CONTAINS, no solutions
    | d == (r0 + r1) = Just [Point x2 y2]         -- INTERSECTION in single point
    | otherwise = Just [Point xi1 yi1, Point xi2 yi2]   -- INTERSECTION: two solutions
    where
        (dx, dy) = (x1 - x0, y1 - y0)
        d = sqrt(dx*dx + dy*dy)
        a = ((r0*r0) - (r1*r1) + (d*d)) / (2.0 * d)
        h = sqrt((r0*r0) - (a*a))
        x2 = x0 + (dx * a/d)
        y2 = y0 + (dy * a/d)
        rx = -dy * (h/d)
        ry = dx * (h/d)
        xi1 = x2 + rx
        yi1 = y2 + ry
        xi2 = x2 - rx
        yi2 = y2 - ry