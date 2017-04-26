{-
******************************************************************************
*                          Y F R O B / C O M M O N                           *
*                                                                            *
*	Module:		RobotIO					             *
*	Purpose:	Type classes and related definitions for robot I/O.  *
*	Author:		Henrik Nilsson					     *
*									     *
******************************************************************************
-}

-- These classes are to be instatiated by various I/O types for specific
-- robots, thus allowing generic controller code working across a range
-- of robots to be written.

-- To do/think about:
-- * Maybe classes/parts of classes providing access to constants ought
--   to be separated from true input classes. A robot XXX would then
--   have a record XXXConstants instantiating the relevant constant classes,
--   and XXXInput instantiating the relevant input classes. A typical
--   controller would have a type like
--   ctrlr :: (HasRobotProperties c, ..., HasOdometry i, ...) => c -> SF i o
--   Problem: some classes contain both constant aspects and true, variable
--   entities. The separation into constant and variable entities could be
--   rather awkward.
--   Furthermore! The geometry of a robot could potentially change dynamically,
--   things like maximal acceleration could certainly change (e.g. depending
--   on the battery status), and so on. So maybe it isn't really to bad to
--   make these "constants" part of the input.
-- * Maybe _some_ of this should move into Yampa at some point?
--   E.g. if one wants a "standardized" way of doing console I/O and GUI I/O?
--   Or is this strictly the business of the application specific layer
--   around Yampa?
-- * (This is also Yampa-related: the note also appears in YampaEvent.) 
--   Question: How do we handle "events" from the outside world? Do we allow
--   events from the outside world in the first place??? Currently, the
--   answer is "no" because Event is an abstract type, and there are
--   (intentionally) no constructors for constructing events pointwise, e.g.
--   from a Bool or Maybe. Instead, events are the result of (stateful) edge
--   detection.
--   There are a number of reasons for this:
--   - Potentially, we may be semantically better off by not committing to an
--     Maybe/Event isomorphism by providing an interface which implies such
--     a relationship. Whether the present interface commits to this
--     isomorphism anyway is an open question.
--   - Events are supposed to occur instantaneously. Can one rely on the
--     outside world to maintain this invariant in general? As long as
--     we maintain control of event generation inside Yampa, this is a
--     non-issue.
--   - When observing events from the outside for the purpose of switching,
--     we often want to do that through an edge detector anyway. Otherwise,
--     if we simply observed an event signal generated from the outside and
--     did an immediate switch, then the event condition would remain AFTER
--     the switch, potentially causeing another immediate switch. This is of
--     course no reason to not allow events from the outside in themselves
--     (one could always do edge detection anyway), but just to say that
--     events from the outside does not seem that useful.
--   Regarding the last point, note that we'll have that problem anytime
--   we feed events into a signal transformer: if it does immediate switching
--   internally, the event condition will persist after the switch.


module FRP.YFrob.Common.RobotIO where

import FRP.Yampa
import FRP.Yampa.MergeableRecord
import FRP.YFrob.Common.PhysicalDimensions


------------------------------------------------------------------------------
-- Types related to the interface classes
------------------------------------------------------------------------------

type RobotType = String	-- "SimbotA", "SimbotB", possibly  "Pioneer" ...
type RobotId   = Int


data BatteryStatus = BSHigh
                   | BSLow
                   | BSCritical
		   deriving (Eq, Show)


------------------------------------------------------------------------------
-- Property classes and related utility functions/signal transformers
------------------------------------------------------------------------------

class HasRobotProperties p where
    rpType     :: p -> RobotType	-- Type of the robot, e.g. "SimbotA".
    rpId       :: p -> RobotId		-- Identity of the robot.
    rpDiameter :: p -> Length		-- Distance between the wheels.
    rpAccMax   :: p -> Acceleration	-- Maximal translational acc.
    rpWSMax    :: p -> Speed		-- Maximal peripheral wheel speed.


------------------------------------------------------------------------------
-- Input classes and related utility functions/signal transformers
------------------------------------------------------------------------------

class HasSystemTime i where
    stSystemTime :: i -> Time		-- Time since system start.


-- Similar to "localTime".
systemTime :: HasSystemTime i => SF i Time
systemTime = arr stSystemTime


-- Note: Don't make any assumptions about how long the stuck condition
-- will persist. It could be quite transient, e.g. only while trying to
-- move and getting nowhere, or it could even be a "true event", such as
-- a flag indicating that the condition has occurred which is reset
-- by the act of reading. But if observed through edge detectors such as
-- those defined below, this should not matter.

class HasRobotStatus i where
    rsBattStat :: i -> BatteryStatus	-- Curent battery status.
    rsIsStuck  :: i -> Bool		-- Currently stuck or not.


rsBattStatChanged :: HasRobotStatus i => SF i (Event BatteryStatus)
rsBattStatChanged =
    arr rsBattStat
    >>> edgeBy (\bs bs' -> if bs == bs' then Nothing else Just bs') BSHigh


rsBattStatLow :: HasRobotStatus i => SF i (Event ())
rsBattStatLow = arr (\i -> rsBattStat i == BSLow) >>> edge 


rsBattStatCritical :: HasRobotStatus i => SF i (Event ())
rsBattStatCritical = arr (\i -> rsBattStat i == BSCritical) >>> edge 


rsStuck :: HasRobotStatus i => SF i (Event ())
rsStuck = arr rsIsStuck >>> edge


class HasOdometry i where
    odometryPosition :: i -> Position2	-- Current position.
    odometryHeading  :: i -> Heading	-- Current heading.


-- Simple abstraction over a variety of devices capable of providing range
-- information (laser rangefinders, sonar, omnicam, ...). (The word "range"
-- is used in the meaning "distance to target"). rfRange provides an
-- omnidirectional range map giving the (estimated) distance to the closest
-- obstacle for any angle, thus hiding underlying details concerning sensor
-- types, their number and directions, the field of view for each, etc.
-- At least the range information for 0, pi/2, -pi/2, pi are accurate
-- (no instance should be provided for this class otherwise).
-- rfMaxRange gives (the upper limit of) the maximal target distance the
-- underlying device(s) is capable of detecting. Any larger reported distance
-- means "no target in range".

class HasRangeFinder i where
    rfRange :: i -> Angle -> Distance
    rfMaxRange :: i -> Distance


-- Used to indicate out of range signals. But to test for this, just compare
-- against rfMaxRange.
rfOutOfRange :: Distance
rfOutOfRange = 1.0e100


rfFront :: HasRangeFinder i => i -> Distance
rfFront i = rfRange i 0.0

rfBack :: HasRangeFinder i => i -> Distance
rfBack i = rfRange i pi

rfLeft :: HasRangeFinder i => i -> Distance
rfLeft i = rfRange i (pi/2)

rfRight :: HasRangeFinder i => i -> Distance
rfRight i = rfRange i (-pi/2)


-- Interface to sensors that keep track of animate objects, i.e. other
-- robots and balls. Could be an overhead camera, or perhaps all animate
-- objects are equipped with some kind of beacon that makes it possible to
-- track the other robots, or maybe all robots broadcast their positions over
-- radio. Anyway, this is a rather platform-specific interface, and should
-- probably be moved to the platform-specific part of the library at some
-- point.
--
-- For each other robot, the robot type, the angle (relative to own heading),
-- and distance is provided. The distance is center to center. For each ball,
-- the relative angle and distance is provided.

class HasAnimateObjectTracker i where
    aotOtherRobots :: i -> [(RobotType, RobotId, Angle, Distance)]
    aotBalls       :: i -> [(Angle, Distance)]


-- Very simple textual console input. Not good enough for handling e.g.
-- shift, ctrl, function keys, ... Currently somewhere between event-land
-- and continous land. Should not assume that a key only occurs momentarily
-- (see comments about events above), even if that's likely what's going on.
-- On the other hand, if the interface truly represented a continuous view
-- of a keyboard, the type would be a LIST (possibly empty) of KEYS currently
-- being down. But that might be a bit fragile (leading to "stuck keys").
-- So one would probably have to also subscribe to focus events to clear
-- the list? Maybe such sophistication is more appropriate for a GUI input
-- class?

class HasTextualConsoleInput i where
    tciKey :: i -> Maybe Char


-- Detects new key presses without insisting on all keys being released
-- in between (2-key rollover). Thus needs to be initialized with the
-- previous key status.
tciNewKeyDown :: HasTextualConsoleInput i => Maybe Char -> SF i (Event Char)
tciNewKeyDown mk = arr tciKey >>> edgeBy newKeyDown mk
    where
        newKeyDown Nothing  Nothing                   = Nothing
        newKeyDown Nothing  mk'@(Just _)              = mk'
        newKeyDown (Just k) mk'@(Just k') | k' /= k   = mk'
                                          | otherwise = Nothing
        newKeyDown (Just _) Nothing                   = Nothing


-- Detects key presses, insisting on all keys being released in between
tciKeyDown :: HasTextualConsoleInput i => SF i (Event Char)
tciKeyDown = arr tciKey >>> edgeJust


------------------------------------------------------------------------------
-- Output classes and related types and utility functions/signal transformers
------------------------------------------------------------------------------

class MergeableRecord o => HasDiffDrive o where
    -- Brake both wheels.
    ddBrake   :: MR o

    -- Set wheel velocities individually.
    ddVelDiff :: Velocity -> Velocity -> MR o

    -- Set wheel velocities in terms of overall transl. and rot. velocity.
    ddVelTR   :: Velocity -> RotVel -> MR o


class MergeableRecord o => HasTextConsoleOutput o where
    tcoPrintMessage :: Event String -> MR o
