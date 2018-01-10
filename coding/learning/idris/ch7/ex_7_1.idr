-- copied from ch4   
||| Represents Shapes
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

-- copied from ch4            
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle x y) = x * y
area (Circle r) = pi * r * r

Eq Shape where
  (==) (Triangle b1 h1)   (Triangle b2 h2)  = b1 == b2 && h1 == h2
  (==) (Rectangle l1 h1)  (Rectangle l2 h2) = l1 == l2 && h1 == h2
  (==) (Circle r1)        (Circle r2)       = r1 == r2
  (==) _                  _                 = False

Ord Shape where
  compare s1 s2 = let a1 = area s1
                      a2 = area s2 in
                      compare a1 a2

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]