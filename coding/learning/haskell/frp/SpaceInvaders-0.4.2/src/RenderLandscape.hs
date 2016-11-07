{- $Id: RenderLandscape.hs,v 1.3 2004/11/18 13:02:26 henrik Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:         RenderLandscape					     *
*       Purpose:        Rendering of the fixed backdrop.		     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module RenderLandscape (
    landscape		-- :: HGL.Graphic
) where

import Data.Array
import qualified Graphics.HGL as HGL

import FRP.Yampa.Point2 (Point2(..))

import WorldGeometry
import Colors
import ColorBindings


------------------------------------------------------------------------------
-- Landscape rendering
------------------------------------------------------------------------------

landscape :: HGL.Graphic
landscape =
    HGL.mkBrush (colorTable ! distantMountainColor) $ \dmcBrush ->
    HGL.mkBrush (colorTable ! closeMountainColor) $ \cmcBrush ->
    HGL.mkBrush (colorTable ! groundColor) $ \groundBrush ->
    HGL.overGraphics
	[ HGL.withBrush groundBrush $ HGL.polygon groundPoints,
          HGL.withBrush cmcBrush    $ HGL.polygon cmPoints,
          HGL.withBrush dmcBrush    $ HGL.polygon dmPoints
	]


-- Points defining the distant mountain chain.
dmPoints :: [HGL.Point]
dmPoints = map relativeToGPoint
               [ (0.00, 0.00),
                 (0.00, 0.62),
                 (0.11, 0.20),
                 (0.42, 0.33),
                 (0.51, 0.24),
                 (0.60, 0.37),
                 (0.68, 0.17),
                 (0.81, 0.26),
                 (0.94, 0.39),
                 (1.00, 0.50),
                 (1.00, 0.00)
               ]

-- Points defining the close mountain chanin.
cmPoints :: [HGL.Point]
cmPoints = map relativeToGPoint
               [ (0.00, 0.00),
                 (0.15, 0.25),
                 (0.35, 0.00),
                 (0.80, 0.20),
                 (0.90, 0.00)
               ]

-- Points defining the ground.
groundPoints :: [HGL.Point]
groundPoints = map relativeToGPoint
                   [ (0.00, 0.00),
                     (0.00, 0.05),
                     (1.00, 0.05),
                     (1.00, 0.00)
                   ]


relativeToGPoint :: (Double, Double) -> HGL.Point
relativeToGPoint (x,y) = (round (x * fromIntegral worldSizeX),
                          round ((1.0 - y) * fromIntegral worldSizeY))
