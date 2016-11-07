{-
******************************************************************************
*                          Y F R O B / C O M M O N                           *
*                                                                            *
*	Module:		PhysicalDimensions				     *
*	Purpose:	Type synonyms for physical dimensions and some	     *
*			related operations.				     *
*	Author:		Henrik Nilsson					     *
*									     *
******************************************************************************
-}

module FRP.YFrob.Common.PhysicalDimensions (
    YFrobReal,

-- One dimensional
    Frequency,
    Mass,
    Length,
    Distance,
    Position,
    Speed,
    Velocity,
    Acceleration,
    Angle,
    Heading,
    Bearing,
    RotVel,
    RotAcc,

-- Two dimensional
    Distance2,
    Position2,
    Velocity2,
    Acceleration2,

-- Three dimensional
    Distance3,
    Position3,
    Velocity3,
    Acceleration3,

-- Operations
    normalizeAngle,	-- :: Angle -> Angle
    normalizeHeading,	-- :: Heading -> Heading
    bearingToHeading,	-- :: Bearing -> Heading
    headingToBearing	-- :: Heading -> Bearing
) where

import FRP.Yampa (Time)
import FRP.Yampa.Miscellany (fMod)
import FRP.Yampa.Geometry (Vector2, Vector3, Point2, Point3)


-- Many of the physical dimensions below are related to time, and variables
-- of these types can thus be expected to occur in numerical expressions along
-- with variables of type time. To facilitate things, they should thus share
-- the same representation. Maybe it is a mistake that Yampa has fixed the
-- type of Time (currently to Double)?

-- Dimensionless type. Same representation as AFRP's Time.
type YFrobReal = Time

------------------------------------------------------------------------------
-- One-dimensional types
------------------------------------------------------------------------------

type Frequency    = YFrobReal -- [Hz]
type Mass         = YFrobReal -- [kg]
type Length       = YFrobReal -- [m]
type Position     = YFrobReal -- [m]	(absolute)
type Distance     = YFrobReal -- [m]	(relative)
type Speed        = YFrobReal -- [m/s]	(unsigned, speed = abs(velocity))
type Velocity     = YFrobReal -- [m/s]	(signed)
type Acceleration = YFrobReal -- [m/s^2]
type Angle        = YFrobReal -- [rad]	(relative)
type Heading      = YFrobReal -- [rad]	(angle relative to x-axis = east)
type Bearing	  = YFrobReal -- [deg]	(compass direction, 0 = N, 90 = E)
type RotVel       = YFrobReal -- [rad/s]
type RotAcc       = YFrobReal -- [rad/s^2]


------------------------------------------------------------------------------
-- Two-dimensional types
------------------------------------------------------------------------------

type Position2     = Point2 Position			-- [m]     (absolute)
type Distance2     = Vector2 Distance			-- [m]     (relative)
type Velocity2     = Vector2 Velocity			-- [m/s]
type Acceleration2 = Vector2 Acceleration		-- [m/s^2]


------------------------------------------------------------------------------
-- Three-dimensional types
------------------------------------------------------------------------------

type Position3     = Point3 Position			-- [m]     (absolute)
type Distance3     = Vector3 Distance			-- [m]     (relative)
type Velocity3     = Vector3 Velocity			-- [m/s]
type Acceleration3 = Vector3 Acceleration		-- [m/s^2]


------------------------------------------------------------------------------
-- Operations
------------------------------------------------------------------------------

-- The resulting angle is in the interval [-pi, pi).
normalizeAngle :: Angle -> Angle
normalizeAngle d = fMod (d + pi) (2 * pi) - pi


-- The resulting heading is in the interval [-pi, pi).
normalizeHeading :: Heading -> Heading
normalizeHeading =  normalizeAngle


-- Bearings in degrees are understood as on a compass; i.e., north is 0,
-- east is 90, south is 180, west is 270.
-- Heading is understood as the angle (in radians) relative to the "x-axis"
-- which is supposed to point East.

-- The resulting heading is in the interval [-pi, pi).
bearingToHeading :: Bearing -> Heading
bearingToHeading b = (fMod (270 - b) 360 - 180) * pi / 180


-- The resulting bearing is in the interval [0, 360).
headingToBearing :: Heading -> Bearing
headingToBearing d = fMod (90 - d * 180 / pi) 360
