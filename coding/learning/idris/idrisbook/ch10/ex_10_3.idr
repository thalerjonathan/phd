import DataStore

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $
            addToStore ("Second", 2) $
            empty

getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) 
    = value :: getValues store | rec

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

triangle : Double -> Double -> Shape
triangle = Triangle

rectangle : Double -> Double -> Shape
rectangle = Rectangle

circle : Double -> Shape
circle = Circle

data ShapeView : Shape -> Type where
  STriangle : ShapeView (triangle base height)
  SRectangle : ShapeView (rectangle x y)
  SCircle : ShapeView (circle r)

shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle
shapeView (Rectangle x y)        = SRectangle
shapeView (Circle x)             = SCircle

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = base * height * 0.5
  area (rectangle x y) | SRectangle = x * y
  area (circle r) | SCircle = pi * r * r
