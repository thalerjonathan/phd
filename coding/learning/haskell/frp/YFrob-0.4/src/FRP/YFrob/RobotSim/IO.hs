{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*	Module:		IO						     *
*	Purpose:	RobotSim I/O types and instances.		     *
*	Author:		Henrik Nilsson					     *
*									     *
******************************************************************************
-}

-- RobotSim-specific I/O types and I/O class instances.
--
-- Comment from (old) PioneerIO: applies to RobotSim as well:
-- Not quite sure any more it is such a great idea to make individual robot
-- types, even specific configurations of such robots, part of the YFRob
-- framework.
-- The alternative would be to make YFrob quite self contained (including
-- what's now in Common, and possibly a simulator), and moving all this stuff
-- to the application level since it is quite unlikely that this stuff is
-- suficiently general to merit being put into a library.
-- 
-- "Simbot" refers to a specific kind of simulated robot, intended to be
-- a fairly generic platform providing aspects common to many typical, real
-- platforms. One could imagine having other types of simulated robots as
-- well, e.g. simulated Pioneers, in which case this module would define
-- PioneerInput/PioneerOutput etc.
--
-- SimbotInput does not currently instantiate HasTextualConsoleInput.
-- One could imagine providing a GUI mechanism to select the simulated robot
-- which is to receive the current console input.

module FRP.YFrob.RobotSim.IO (
    SimbotProperties(..),
    SimbotInput(..),
    SimbotOutput(..),
    DriveMode(..),
    rfIndex
) where

import Data.Ix (rangeSize)
import Data.Array (Array, bounds, (!))

import FRP.Yampa (Time, Event, noEvent, mergeBy)
import FRP.Yampa.MergeableRecord (MergeableRecord(..), MR, mrMake)
import FRP.YFrob.Common.PhysicalDimensions
import FRP.YFrob.Common.RobotIO


------------------------------------------------------------------------------
-- Property type and instances
------------------------------------------------------------------------------

data SimbotProperties = SP {
    -- Fields for HasRobotProperties
    spRType    :: RobotType,	-- "SimbotA" and "SimbotB"
    spRId      :: RobotId,
    spDiameter :: Length,
    spAccMax   :: Acceleration,
    spWSMax    :: Speed
}


instance HasRobotProperties SimbotProperties where
    rpType     = spRType
    rpId       = spRId
    rpDiameter = spDiameter
    rpAccMax   = spAccMax
    rpWSMax    = spWSMax


------------------------------------------------------------------------------
-- Input type and instances
------------------------------------------------------------------------------

-- !!! Should the fields be strict?
-- !!! But it is nice that potentially expensive computations are not
-- !!! carried out unless needed.


-- !!! Should some of the fields, like the field for stuck, really be
--     an event?

data SimbotInput = SI {
    -- Fields for HasSystemTime
    siSystemTime  :: Time,

    -- Fields for HasRobotStatus
    siBattStat    :: BatteryStatus,
    siIsStuck     :: Bool,		-- !!! Should be an event?

    -- Fields for HasOdometry
    siPosition    :: Position2,
    siHeading     :: Heading,

    -- Fields for HasRangeFinder
    siRanges      :: Array Int Distance,  -- n equispaced range finders, CCW.
    siMaxRange    :: Distance,

    -- Fields for HasAnimateObjectTracker
    siOtherRobots :: [(RobotType, RobotId, Angle, Distance)],
    siBalls       :: [(Angle, Distance)]
}


instance HasSystemTime SimbotInput where
    stSystemTime = siSystemTime


instance HasRobotStatus SimbotInput where
    rsBattStat = siBattStat
    rsIsStuck  = siIsStuck


instance HasOdometry SimbotInput where
    odometryPosition = siPosition
    odometryHeading  = siHeading


instance HasRangeFinder SimbotInput where
    rfRange si phi =
        case n of
	    0 -> rfOutOfRange
	    _ -> rs ! rfIndex n phi 
        where
	    rs = siRanges si
	    n  = rangeSize (bounds rs)

    rfMaxRange = siMaxRange


-- Utility function for computing index into range-finder array.
rfIndex :: Int -> Angle -> Int
rfIndex n phi = round (fromIntegral n * phi / (2 * pi)) `mod` n


instance HasAnimateObjectTracker SimbotInput where
    aotOtherRobots = siOtherRobots
    aotBalls       = siBalls


------------------------------------------------------------------------------
-- Output type with instances and subordinate types
------------------------------------------------------------------------------

-- !!! Could be extended with other kinds of control modes.

data DriveMode =
      DMBrake				-- Brake both wheels.
    | DMDiff {
	  dmdLWV :: Velocity,		-- Left Wheel Velocity.
	  dmdRWV :: Velocity		-- Right Wheel Velocity.
      }
    | DMTR {
	  dmtrTV :: Velocity,		-- Translational Velocity.
	  dmtrRV :: RotVel		-- Rotational Velocity.
      }


data SimbotOutput = SO {
    soDM   :: DriveMode,	-- Drive mode output.
    soTCO  :: Event [String]	-- Text console output.
--    soFVGO :: SimpleGraphic	-- FVision GUI output.
}


instance MergeableRecord SimbotOutput where
    mrDefault = SO {soDM = DMBrake, soTCO = noEvent}


instance HasDiffDrive SimbotOutput where
    ddBrake = mrMake (\o -> o {soDM = DMBrake})

    ddVelDiff lwv rwv =
        mrMake (\o -> o {soDM = DMDiff {dmdLWV = lwv, dmdRWV = rwv}})

    ddVelTR v rv =
        mrMake (\o -> o {soDM = DMTR {dmtrTV = v, dmtrRV = rv}})


instance HasTextConsoleOutput SimbotOutput where
    tcoPrintMessage em =
        mrMake (\o -> o {soTCO = mergeBy (++) (soTCO o) (fmap (:[]) em)})


{-
instance HasFVisionGUIOutput SimbotOutput where
    fvgoOverlaySimpleGraphic sg =
	mrMake (\o -> o {soFVGO = sg `SGOver` soFVGO o})
-}
