module Shape

||| Represents Shapes
public export
data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
             Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

export
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle x y) = x * y
area (Circle r) = pi * r * r