module Picture

import Shape

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle)
                       (Translate 15 25 triangle))

% name Shape shape, shape1, shape2
% name Picture pic, pic1, pic2

total
pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

data Biggest = NoTriangle | Size Double

total
compareTriangle : (tleft : Biggest) -> (tright : Biggest) -> Biggest
compareTriangle NoTriangle NoTriangle = NoTriangle
compareTriangle NoTriangle (Size x) = Size x
compareTriangle (Size x) NoTriangle = Size x
compareTriangle (Size x) (Size y) = if x > y then Size x else Size y

total
biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size (area (Triangle x y))
biggestTriangle (Primitive (Rectangle x y)) = NoTriangle
biggestTriangle (Primitive (Circle x)) = NoTriangle
biggestTriangle (Combine pic pic1) = let tleft = biggestTriangle pic
                                         tright = biggestTriangle pic1 in
                                         compareTriangle tleft tright
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
