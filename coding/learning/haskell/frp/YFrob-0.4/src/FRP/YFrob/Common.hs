{-
******************************************************************************
*                          Y F R O B / C O M M O N                           *
*                                                                            *
*	Module:		Common						     *
*	Purpose:	Top level module for the generic part of YFrob 	     *
*	Author:		Henrik Nilsson					     *
*									     *
******************************************************************************
-}

module FRP.YFrob.Common (
    module FRP.Yampa,
    module FRP.Yampa.Task,
    module FRP.Yampa.Geometry,		-- Used for PhysicalDimensions
    module FRP.Yampa.MergeableRecord,
    module FRP.YFrob.Common.Diagnostics,
    module FRP.YFrob.Common.PhysicalDimensions,
    module FRP.YFrob.Common.RobotIO
) where


-- Yampa and Yampa extensions that are judged sufficiently useful in the YFrob
-- conext to be provided by default.
import FRP.Yampa
import FRP.Yampa.Task
import FRP.Yampa.Geometry hiding ((*^), (^+^), (^-^), (^/), dot, negateVector,
                                  norm, normalize, zeroVector, VectorSpace)
import FRP.Yampa.MergeableRecord

-- YFrob modules.
import FRP.YFrob.Common.Diagnostics
import FRP.YFrob.Common.PhysicalDimensions
import FRP.YFrob.Common.RobotIO
