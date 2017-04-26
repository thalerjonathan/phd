{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:         RenderFixedWalls				     *
*       Purpose:        Rendering of the fixed walls.			     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.RenderFixedWalls (
    fixedWalls		-- :: HGL.Graphic
) where

import Data.Array
import qualified Graphics.HGL as HGL

import FRP.Yampa.Point2 (Point2(..))

import FRP.YFrob.RobotSim.WorldGeometry
import FRP.YFrob.RobotSim.Colors
import FRP.YFrob.RobotSim.ColorBindings


------------------------------------------------------------------------------
-- Fixed wall rendering
------------------------------------------------------------------------------

-- Currently, the only fixed walls are the outer ones.

fixedWalls :: HGL.Graphic
fixedWalls =
    HGL.mkBrush (colorTable ! outerWallColor) $ \brush ->
    HGL.withBrush brush $
    -- Drawing rectangles seems slightly quicker than a complex polygon.
    -- HGL.polygon [pN1, pN2, pN3, pS4, pS1, pW2, pW3, pW4, pE1, pE2]
    HGL.overGraphics
	[ HGL.polygon [pN1, pN2, pN3, pN4],
	  HGL.polygon [pE1, pE2, pE3, pE4],
	  HGL.polygon [pS1, pS2, pS3, pS4],
	  HGL.polygon [pW1, pW2, pW3, pW4]
	]
    where
	pN1 = position2ToGPoint (Point2 worldXMin worldNorthWall)
	pN2 = position2ToGPoint (Point2 worldXMin worldYMax)
	pN3 = position2ToGPoint (Point2 worldXMax worldYMax)
	pN4 = position2ToGPoint (Point2 worldXMax worldNorthWall)

	pE1 = position2ToGPoint (Point2 worldEastWall worldSouthWall)
	pE2 = position2ToGPoint (Point2 worldEastWall worldNorthWall)
	pE3 = position2ToGPoint (Point2 worldXMax worldNorthWall)
	pE4 = position2ToGPoint (Point2 worldXMax worldSouthWall)
	
	pS1 = position2ToGPoint (Point2 worldXMin worldYMin)
	pS2 = position2ToGPoint (Point2 worldXMin worldSouthWall)
	pS3 = position2ToGPoint (Point2 worldXMax worldSouthWall)
	pS4 = position2ToGPoint (Point2 worldXMax worldYMin)

	pW1 = position2ToGPoint (Point2 worldXMin worldSouthWall)
	pW2 = position2ToGPoint (Point2 worldXMin worldNorthWall)
	pW3 = position2ToGPoint (Point2 worldWestWall worldNorthWall)
	pW4 = position2ToGPoint (Point2 worldWestWall worldSouthWall)
	

-- For some reason, using regions does not seem to work (ONLY walls visible.)
{-
walls :: Graphic
walls =
    HGL.mkBrush (colorTable ! DimGrey) $ \brush ->
    HGL.withBrush brush $
    HGL.regionToGraphic $
        HGL.subtractRegion (HGL.rectangleRegion p1 p2)
		           (HGL.rectangleRegion p3 p4)
    where
	p1 = position2ToGPoint (Point2 worldXMin worldYMin)
	p2 = position2ToGPoint (Point2 worldXMax worldYMax)
	p3 = position2ToGPoint (Point2 worldWestWall worldSouthWall)
	p4 = position2ToGPoint (Point2 worldEastWall worldNorthWall)
-}
