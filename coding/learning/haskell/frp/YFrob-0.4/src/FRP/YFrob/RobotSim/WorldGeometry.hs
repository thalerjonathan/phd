{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		WorldGeometry					     *
*       Purpose:	Constants and functions defining the geometry of     *
*			the world.					     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.WorldGeometry where

import FRP.Yampa.Geometry (Point2(..))
-- import AFRPTransform2	-- Not yet written
import FRP.YFrob.Common.PhysicalDimensions
import qualified Graphics.HGL as HGL (Point)


-- Everything in the world is measured in meters.

pixelsPerMeter :: YFrobReal
pixelsPerMeter = 60.0

pixelsToMeters :: Int -> Length
pixelsToMeters p = (fromIntegral p) / pixelsPerMeter 

metersToPixels :: Length -> Int
metersToPixels m = round (m * pixelsPerMeter)


-- The world is assumed to be rectangular.

worldXMin, worldXMax :: Position
worldYMin, worldYMax :: Position
worldXMin = -5.0
worldYMin = -5.0
worldXMax = 5.0
worldYMax = 5.0


-- World size in pixels.

worldSizeX, worldSizeY :: Int
worldSizeX = metersToPixels (worldXMax - worldXMin)
worldSizeY = metersToPixels (worldYMax - worldYMin)


-- Positions of the walls.

worldNorthWall, worldSouthWall :: Position
worldEastWall, worldWestWall :: Position
worldNorthWall = worldYMax - 0.2
worldEastWall  = worldXMax - 0.2
worldSouthWall = worldYMin + 0.2
worldWestWall  = worldXMin + 0.2


-- Co-ordinate translations

-- Re-visit these definitions if/once affine transforms are introduced in
-- Yampa.
-- Maybe use affine transformations also for the basic conversions HGL.Point
-- <-> Position2?

{-
pointToPositionT :: Transform2
pointToPositionT = translate2 (vector2XY worldXMin worldYMax) `compose2` 
                   uscale2 (1 / pixelsPerMeter) `compose2` 
                   mirrorY2 
-}


gPointToPosition2 :: HGL.Point -> Position2
gPointToPosition2 (x, y) = (Point2 (pixelsToMeters x + worldXMin)
				   (worldYMax - pixelsToMeters y))


{-
positionToPointT :: Transform2
positionToPointT = uscale2 pixelsPerMeter `compose2`
                   translate2 (vector2XY (-worldXMin) worldYMax) `compose2`
                   mirrorY2
-}


position2ToGPoint :: Position2 -> HGL.Point
position2ToGPoint (Point2 x y) =
    (metersToPixels (x - worldXMin), metersToPixels (worldYMax - y))
