module FRP.FrABS.Environment.Continuous (
    distanceManhattanCont2D,
    distanceEuclideanCont2D,

    wrapCont2D
  ) where

data Continuous2DEnvironmentData = Continuous2DEnvironmentData {
    envAgentPosition :: Continuous2DCoord,

    
}

data Continuous2DEnvironmentData = Continuous2DEnvironmentData {
    envBehaviour :: Maybe (EnvironmentBehaviour e),
    envLimits :: Continuous2DLimit,
    envWrapping :: EnvironmentWrapping,
    envRng :: StdGen
}

distanceManhattanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceManhattanCont2D (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))

distanceEuclideanCont2D :: Continuous2DCoord -> Continuous2DCoord -> Double
distanceEuclideanCont2D (x1, y1) (x2, y2) = sqrt (xDelta*xDelta + yDelta*yDelta)
    where
        xDelta = x2 - x1
        yDelta = y2 - y1

wrapCont2D :: Continuous2DLimit -> EnvironmentWrapping -> Continuous2DCoord -> Continuous2DCoord
wrapCont2D (maxX, maxY) ClipToMax (x, y) = (max 0 (min x maxX), max 0 (min y maxY))
wrapCont2D l@(maxX, _) WrapHorizontal (x, y)
    | x < 0 = wrap l WrapHorizontal (x + maxX, y)
    | x >= maxX = wrap l WrapHorizontal (x - maxX, y)
    | otherwise = (x, y)
wrapCont2D l@(_, maxY) WrapVertical (x, y)
    | y < 0 = wrap l WrapVertical (x, y + maxY)
    | y >= maxY = wrap l WrapVertical (x, y - maxY)
    | otherwise = (x, y)
wrapCont2D l WrapBoth coord = wrap l WrapHorizontal $ wrap l WrapVertical coord