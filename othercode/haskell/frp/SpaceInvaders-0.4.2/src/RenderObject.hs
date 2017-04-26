{- $Id: RenderObject.hs,v 1.3 2004/11/18 13:02:26 henrik Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:		RenderObject					     *
*       Purpose:	Object rendering.				     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module RenderObject (
    renderObjects	-- :: [ObjObjState] -> HGL.Graphic
) where

import Data.Array
import qualified Graphics.HGL as HGL
import FRP.Yampa.Geometry

import PhysicalDimensions
import WorldGeometry
import Colors
import ColorBindings
import Object

------------------------------------------------------------------------------
-- Object rendering
------------------------------------------------------------------------------

-- This interface allows optimization. E.g. pen/brush creation can be
-- lifted to the top level.

renderObjects :: [ObsObjState] -> HGL.Graphic
renderObjects ooss = HGL.overGraphics (map renderObject ooss)


renderObject :: ObsObjState -> HGL.Graphic
renderObject (OOSGun {oosPos = p, oosAmLvl = l}) =
    centeredText Green (p .-^ vector2 0 (gunHeight/2)) (show l)
    `HGL.overGraphic` triangle gunColor p1 p2 p3
    where
	p1 = p .+^ vector2 0 (gunHeight/2)
        p2 = p .+^ vector2 (-(gunBase/2)) (-(gunHeight/2))
        p3 = p .+^ vector2 (gunBase/2) (-(gunHeight/2))
renderObject (OOSMissile {oosPos = p}) = circle missileColor p missileRadius
renderObject (OOSAlien {oosPos = p, oosHdng = h}) =
    line alienWingColor p1 p2
    `HGL.overGraphic` (circle alienDoorColor p3 (alienRadius/3))
    `HGL.overGraphic` (circle alienColor p alienRadius)
    where
	p1 = p .+^ v
	p2 = p .-^ v
        p3 = p .+^ vector2Polar (alienRadius / 2) h
        v  = vector2Polar alienWingRadius (h + pi/2)


line :: Color -> Position2 -> Position2 -> HGL.Graphic
line c p1 p2 =
    -- Line style and thiknes seems to be ignored completely?
    HGL.mkPen HGL.Dash 2 (colorTable ! c) $ \pen ->
    HGL.withPen pen			      $
    HGL.line gp1 gp2
    where
        gp1 = position2ToGPoint p1
	gp2 = position2ToGPoint p2


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


{-
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
-}

centeredText :: Color -> Position2 -> String -> HGL.Graphic
centeredText c p s =
    HGL.withTextColor (colorTable ! c) $
    HGL.withTextAlignment (HGL.Center, HGL.Baseline) $
    HGL.text gp s
    where
	gp = position2ToGPoint p

{-
-- Centering pretty ad hoc.
centeredText :: Position2 -> String -> HGL.Graphic
centeredText p s = HGL.text (x', y') s
    where
	(x,y) = position2ToGPoint p
        x'    = x - (445 * length s) `div` 100
        y'    = y - 7
-}
