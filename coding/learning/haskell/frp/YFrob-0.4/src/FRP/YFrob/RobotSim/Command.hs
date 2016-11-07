{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		Command						     *
*       Purpose:	The simulator command type.			     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.Command (
    Command(..)
) where

import FRP.YFrob.Common.PhysicalDimensions (Angle)
import FRP.YFrob.RobotSim.Object (ObjClass)


data Command =
-- Commands in Edit mode.
      CmdQuit				-- Quit the simulator.
    | CmdRun				-- Run simulation.
    | CmdCreateObst  ObjClass		-- Create an object <: ClsObst
    | CmdCreateRobot ObjClass		-- Create a robot <: ClsRobot.
    | CmdCreateBall			-- Create a ball.
    | CmdDelete				-- Delete an object.
    | CmdSelectNext			-- Select (next) object.
    | CmdSelectPrev			-- Select (previous) object.
    | CmdSelect      ObjClass		-- Select all matching objects.
    | CmdUnselectAll			-- Unselect all objects.
    | CmdTurnLeft			-- Turn robot left.
    | CmdTurnRight			-- Turn robot right.
    | CmdTurnTo      Angle		-- Turn to specified dir. (radians).
    | CmdSave        String             -- Save the world to file path.
    | CmdLoad        String             -- Load a world from file path.
-- Commands in Frozen mode.
    | CmdResume				-- Resume simulation.
    | CmdEdit				-- Terminate sim., go into edit mode.
-- Commands in Running mode.
    | CmdFreeze				-- Freeze simulation.
