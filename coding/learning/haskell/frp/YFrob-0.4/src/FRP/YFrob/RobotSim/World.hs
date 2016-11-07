{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		World						     *
*       Purpose:	The world representation and related definitions.    *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- !!! This module should probably go away. It does not seem to make much
-- !!! sense to share "world definitions" between the simulator and the
-- !!! editor. Potentially they'll have different types, and the simulator
-- !!! already contains a type defintion for a "static world". "ObjId"s
-- !!! are currently generated locally for in simulator and at some point in
-- !!! the editor. Thus they are not shared either. Interaction between
-- !!! simulator and editor are in terms of "WorldTemplate"s. Moreover,
-- !!! "newRobotId" presumably belongs squarely in the editor.

module FRP.YFrob.RobotSim.World (
    ObjId,
    World,
    newRobotId	-- :: ObjClass -> World -> RobotId
) where

import Data.List ((\\))

import FRP.YFrob.Common.Diagnostics (intErr)
import FRP.YFrob.Common.RobotIO (RobotId)

import FRP.YFrob.RobotSim.IdentityList
import FRP.YFrob.RobotSim.Object

type ObjId = ILKey

type World = IL Object


newRobotId :: ObjClass -> World -> RobotId
newRobotId objCls world | objCls <: ClsRobot = head ([0..] \\ allRIdsForClass)
                        | otherwise          = intErr "RSWorld"
                                                      "newRobotId"
                                                      "Bad object class." 
    where
        allRIdsForClass =
	    mapFindAllIL
		(\(_, obj) -> if obj `inClass` objCls then
			          Just (objRId obj)
                              else Nothing)
                world
