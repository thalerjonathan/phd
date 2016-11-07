{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		ColorBindings					     *
*       Purpose:	Definition of colours for various objects.	     *
*       Author:		Henrik Nilsson                                       *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.ColorBindings where

import FRP.YFrob.RobotSim.Colors


------------------------------------------------------------------------------
-- Fixed walls colour bindings
------------------------------------------------------------------------------

outerWallColor	= DarkOliveGreen	-- DarkGrey


------------------------------------------------------------------------------
-- Object colour bindings
------------------------------------------------------------------------------

blockColor	 = DarkKhaki
nsWallColor	 = SlateGrey
ewWallColor	 = SlateGrey
simbotAColor	 = RoyalBlue
simbotAAltColor	 = MediumVioletRed	-- Quick hack
simbotANoseColor = MediumOrchid
simbotBColor	 = SeaGreen
simbotBAltColor  = Goldenrod		-- Quick hack
simbotBNoseColor = NavyBlue
ballColor        = Orange
bboxColor	 = Red
