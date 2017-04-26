{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		RenderObject					     *
*       Purpose:	Object rendering.				     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- ToDo:
-- Add display of RobotId to the robots.

-- Note: Working on the level of signal functions would obviously allow the
-- display of a robot to be animated easily (e.g. cycling colours, flashing
-- bounding boxes). However, the current approach is to map a list of objects
-- directly to a graphic because that, in principle, allows some optimzations
-- that might be important.
--
-- In the future, one might want to work on a higher abstract level than
-- Graphic to facilitate e.g. scaling and rotations. In particular if a
-- library like Haven replaces HGL.


module FRP.YFrob.RobotSim.RenderObject (
    renderObjects	-- :: [Object] -> HGL.Graphic
) where

import Data.Array
import qualified Graphics.HGL as HGL
import FRP.Yampa.Geometry

import FRP.YFrob.Common.PhysicalDimensions

import FRP.YFrob.RobotSim.WorldGeometry
import FRP.YFrob.RobotSim.Object
import FRP.YFrob.RobotSim.Colors
import FRP.YFrob.RobotSim.ColorBindings

------------------------------------------------------------------------------
-- Object rendering
------------------------------------------------------------------------------

-- This interface allows optimization. E.g. pen/brush creation can be
-- lifted to the top level.

renderObjects :: [Object] -> HGL.Graphic
renderObjects objs = HGL.overGraphics (map renderObject objs)


renderObject :: Object -> HGL.Graphic
renderObject (ObjBlock {objSel = s, objBBox = BBox p1 p2}) =
    rectObst s blockColor p1 p2
renderObject (ObjNSWall {objSel = s, objBBox = BBox p1 p2}) =
    rectObst s nsWallColor p1 p2
renderObject (ObjEWWall {objSel = s, objBBox = BBox p1 p2}) =
    rectObst s ewWallColor p1 p2
renderObject (ObjSimbotA {objRId=rid, objSel=s, objPos=p, objHdng=h,
                          objBBox=BBox p1 p2}) =
    -- drawPicUnscaled (positionToPointT %$ pic) `HGL.overGraphic`
    if s then
        (bbox p1 p2) `HGL.overGraphic` simbot
    else
        simbot
    where
        simbot = centeredText p (show rid)
                 `HGL.overGraphic` (circle simbotANoseColor pn 
                                           (simbotARadius/3))
	         `HGL.overGraphic` (circle color p simbotARadius)
        pn = p .+^ (vector2Polar simbotARadius h)
        color = if rid < 10 then simbotAColor else simbotAAltColor
renderObject (ObjSimbotB {objRId = rid, objSel=s, objPos=p, objHdng=h,
                          objBBox=BBox p1 p2}) =
    -- drawPicUnscaled (positionToPointT %$ pic) `HGL.overGraphic`
    if s then
       (bbox p1 p2) `HGL.overGraphic` simbot
    else
        simbot
    where
        simbot = centeredText p (show rid)
                 `HGL.overGraphic` (circle simbotBNoseColor pn 
                                           (simbotBRadius/3))
	         `HGL.overGraphic` (triangle color pb1 pn pb2)
        pn  = p .+^ (vector2Polar simbotBRadius h)
        pb1 = p .+^ (vector2Polar simbotBRadius (h + 2*pi/3))
        pb2 = p .+^ (vector2Polar simbotBRadius (h - 2*pi/3))
        color = if rid < 10 then simbotBColor else simbotBAltColor
renderObject (ObjBall {objSel = s, objPos = p, objBBox = BBox p1 p2}) =
    if s then
        (bbox p1 p2) `HGL.overGraphic` ball
    else
        ball
    where
	ball = circle ballColor p ballRadius


rectObst :: Bool -> Color -> Position2 -> Position2 -> HGL.Graphic
rectObst s c p1 p2 =
    if s then
        (bbox p1 p2) `HGL.overGraphic` (rectangle c p1 p2)
    else
        (rectangle c p1 p2)


triangle :: Color -> Position2 -> Position2 -> Position2 -> HGL.Graphic
triangle c p1 p2 p3 =
    HGL.mkBrush (colorTable ! c) $ \brush ->
    HGL.withBrush brush	      $
    HGL.polygon [gp1, gp2, gp3]
    where
        gp1 = position2ToGPoint p1
	gp2 = position2ToGPoint p2
	gp3 = position2ToGPoint p3


rectangle :: Color -> Position2 -> Position2 -> HGL.Graphic
rectangle c p1 p2 =
    HGL.mkBrush (colorTable ! c) $ \brush ->
    HGL.withBrush brush	      $
    HGL.polygon [gp11, gp12, gp22, gp21]
    where
        gp11@(x1,y1) = position2ToGPoint p1
	gp12	     = (x1, y2)
	gp22@(x2,y2) = position2ToGPoint p2
	gp21	     = (x2, y1)


circle :: Color -> Position2 -> Length -> HGL.Graphic
circle c p r = 
    HGL.mkBrush (colorTable ! c) $ \brush ->
    HGL.withBrush brush	        $
    HGL.ellipse gp11 gp22
    where
        d   = vector2 r r
        gp11 = position2ToGPoint (p .-^ d)
	gp22 = position2ToGPoint (p .+^ d)


bbox :: Position2 -> Position2 -> HGL.Graphic
bbox p1 p2 =
    -- Line style and thiknes seems to be ignored completely?
    HGL.mkPen HGL.Dash 2 (colorTable ! bboxColor) $ \pen ->
    HGL.withPen pen			      $
    HGL.polyline [gp11, gp12, gp22, gp21, gp11]
    where
        gp11@(x1,y1) = position2ToGPoint p1
	gp12	     = (x1, y2)
	gp22@(x2,y2) = position2ToGPoint p2
	gp21	     = (x2, y1)


-- Centering pretty ad hoc.
centeredText :: Position2 -> String -> HGL.Graphic
centeredText p s = HGL.text (x', y') s
    where
	(x,y) = position2ToGPoint p
        x'    = x - (445 * length s) `div` 100
        y'    = y - 7


{-
-- Old stuff. Revisit if picture output is re-introduced.

-- The drawPic routine in Graphics.hs adds an extra bit of scaling.
-- This undoes it. 

drawPicUnscaled pic = FG.drawPic (uscale2 0.01 %$ pic)
-}
