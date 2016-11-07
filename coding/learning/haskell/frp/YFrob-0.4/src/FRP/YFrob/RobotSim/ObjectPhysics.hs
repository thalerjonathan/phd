{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		ObjectPhysics					     *
*       Purpose:	Physics for objects and their interactions.	     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.ObjectPhysics (
    SimbotDynamics,
    BallDynamics,
    simbotDynamics,	-- :: Length -> Acceleration -> Speed -> Position2
			--    -> Heading -> Velocity -> SimbotDynamics
    ballDynamics,	-- :: YFrobReal -> YFrobReal -> Position2 -> Velocity2
			--    -> BallDynamics
    hitFixedWall,	-- :: Object -> Event Velocity
    hit,		-- :: Object -> Object -> Event Velocity
    sonarEchoFixedWall,	-- :: Position2 -> Length -> Heading -> Angle
			--    -> Distance
    sonarEcho,		-- :: Position2 -> Heading -> Length -> Angle -> Object
                	--    -> (Heading, Distance)
) where

import FRP.Yampa
import FRP.Yampa.Utilities (impulseIntegral)
import FRP.Yampa.Geometry
-- Actually, publicly Yampa 0.9.2.3 exports Event non-abstractly.
import FRP.Yampa.Internals (Event(..))	-- Breach of abstraction!

import FRP.YFrob.Common.Diagnostics
import FRP.YFrob.Common.PhysicalDimensions

import FRP.YFrob.RobotSim.WorldGeometry (worldEastWall, worldNorthWall,
			                 worldWestWall, worldSouthWall)
import FRP.YFrob.RobotSim.Object


------------------------------------------------------------------------------
-- Simbot dynamics
------------------------------------------------------------------------------

{-
General structure
-----------------

This is a very simple physical robot model for the Simbots, especially
w.r.t. interaction which is internalized in the model. Ideally, the
physics model would have inputs for both control signals and external
influences, such as impulses (events) for collisions, or external forces
due to someone pushing the robot, etc.

The Simbot physical robot model
-------------------------------

We adopt a simplified robot model which has translational inertia but no
rotational inertia (i.e. all mass is located at the center of gravity).

The robots have two individually controllable wheels which do not slip.
One wheel is located on the left side of the robot, the other wheel on the
right side. The distance between the wheels is known as the robot diameter,
and denoted by d in the following. The true peripheral speed of the wheels
are denoted by v_l and v_r respectively.

The heading h of the robot is given by

   h = h_0 + integral (v_r - v_l) / d

The speed v (in direction h) is given by

   v = (v_r + v_l) / 2				(1)

Thus the position p is given by

   p = p_0 + integral (v@h)

where v@h is the velocity _vector_.

The robot model receives the desired peripheral wheel speeds v_ld and v_rd
from the controller. Of course, these can change instantaneously.

v_rd - v_ld is proportinal to the desired instantaneous change in direction.
Since there is no rotational inertia, this should also be the true
instantaneous change in direction. Thus we have

   v_rd - v_ld = v_r - v_l			(2)

v_rd + v_ld is proportional to the desired translational speed. The
translational speed is subject to inertia and thus cannot change instantan-
eously. We assume a maximal translational acceleration a_max (which is
a function of the mass of the robot, the strengths of the motors etc.),
and that changes in translational speed always is done at that acceleration.

Note that our assumptions unrealisticly imply that the _heading_ can be
changed almost discontinuously. E.g. suppose that v_l = v_r = x. Then by
setting v_ld = -N*x, v_rd = (N+2)*x briefly, where N is a very large number,
the robot could almost turn on the spot despite the inertia. However, by
limiting the peripheral wheel speeds to some maximal value, this should
not be too much of a problem.

Also note that the the assumptions imply that the peripheral speed of
individual wheels does change discontinuously. Thus we cannot compute
any individual acceleration for the wheels.

Instead we proceed as follows. Let a be the instantaneous translational
acceleration (in direction h):

   a = signum (v_rd + v_ld - 2 * v) * a_max

Then the translational speed v is given by:

   v = v_0 + integral a

Now solve (1) and (2) for the individaul true wheel speeds:

   v = (v_r + v_l) / 2				(1)
   v_rd - v_ld = v_r - v_l			(2)

<=> 

   v_rd - v_ld    + 2 * v = 2 * v_r
   -(v_rd - v_ld) + 2 * v = 2 * v_l

<=>

   v_r = v + (v_rd - v_ld)/2
   v_l = v - (v_rd - v_ld)/2 

Thus we get the following control equations:

   a = signum (v_rd + v_ld - 2 * v) * a_max
   v = v_0 + integral a
   v_r = v + (v_rd - v_ld)/2
   v_l = v - (v_rd - v_ld)/2 
   h = h_0 + integral (v_r - v_l) / d
   p = p_0 + integral (v@h)

On rotational inertia
---------------------

One probably can regard rotation around a non-centered axis as translation
combined with rotation around the centered axis. In that case, adding
rotational inertia should be simple. One might want to compute both
translational and rotational inertia from a common mass, assuming some
simple mass distribution, but maybe separate macc acc. and max rotational
acc. will do just as well.

On collision handling
---------------------

Currently thr robot models receives a "collision event". This causes it
to stop and bounce back to where it was before. This has potential problems,
especially when many moving objects interact closely. To counter this, the
event source should generate events not only on an the start of an
overlap condition, but as long as it remains and the relative velocities
would make it worse. "Unphysical"?

In a more sophisticated implementation, the event could carry an impulse
(modelled as a velocity difference since an event as 0 duration). An
interaction resolution algorithm at a higher level could try to figure out
the overall impulse on each object by iteratively consider object-object
interactions until no more collision is found. It is important that the
effect of each found interaction is ADDED BACK to representation of the
state before the checking continues. Effectively this will result in all
interactions being serialised.

The potential drawback of this approach is modularity. It would seem as if
the interaction resolver would have to know a lot about the physics of the
involved objects. Maybe one has to split the physical model into two, one
for continuous dynamics (signal functions), and one for describing how
an object reacts to impulses (ordinary functions, invoked repeatedly during
interaction resolution).

We also have to ensure termination of the resolution algorithm ...

On the inadequacy of the current physical model
-----------------------------------------------

To model object interaction properly, we probably have to restructure the
simulator substantially. On epossibility might be to add extra "ports"
to models, allowing external forces to be taken into account. Alternatively,
one could expose a lot of the internal state (velocity, acceleration) to
allow a generalized "resolve" procedure to compute a suitable new state
after an interaction.

A problem with the latter though, is that it assumes momentary interactions. 
This is just not true. E.g. consider pushing (as opposed to "kicking") a ball. 
A problem with the former is that it assumes that the external forces are
computed separately for interacting objects. This might be diffifult/
impossible. In any event it is wasteful and unsafe since that would prevent
us to take advantage of the law of action/reaction.

Maybe this boils down to solving systems of differential equations after all?
And having different models possibly with different causality for different
interaction cases?

On the assumption that the overall velocity and heading coincide
----------------------------------------------------------------

Note: In a more advanced model, the initial translational velocity
and the output velocity should really be *vectors* since the heading
and where the robot is heading would not necessarily agree, e.g. if
the robot was being pushed.
-}


type SimbotDynamics = SF ((Velocity, Velocity), Event ())
                         (Position2, Heading, Velocity)

type SimbotDynamics' = SF (Velocity, Velocity) (Position2, Heading, Velocity)

-- Simbot dynamics.
-- Arguments:
-- d ..........	Robot diameter.
-- a_max ......	Maximal translational acceleration.
-- ws_max .....	Maximal (peripheral) wheel speed.
-- p_0 ........	Initial position.
-- h_0 ........ Initial heading.
-- v_0 ........ Initial translational velocity.
--
-- Signal inputs:
-- wvs_d.......	Desired wheel velocities.
-- ce .........	Collision event.
--
-- Signal outputs:
-- p ..........	Current position.
-- h .......... Current heading.
-- v ..........	Current translational velocity.

simbotDynamics ::
    Length -> Acceleration -> Speed -> Position2 -> Heading -> Velocity
    -> SimbotDynamics
simbotDynamics d a_max ws_max p_0 h_0 v_0 = proc (wvs_d, ce) -> do
    -- !!! Should be phv@(p, h, v) <- ... but Arrowp refuse.
    rec (p,h,v) <- drSwitch (sd p_0 h_0 v_0) -< (wvs_d, ce')
        p_pre   <- iPre p_0                  -< p
        let ce' = ce `tag` sd p_pre h 0.0
    returnA -< (p, h, v)
    where
        sd = simbotDynamics' d a_max ws_max


-- Interaction-free simbot dynamics.
-- Arguments:
-- d ..........	Robot diameter.
-- a_max ......	Maximal translational acceleration.
-- ws_max .....	Maximal (peripheral) wheel speed.
-- p_0 ........	Initial position.
-- h_0 ........ Initial heading.
-- v_0 ........ Initial translational velocity.
--
-- Signal inputs:
-- v_ld .......	Desired left wheel velocity.
-- v_rd ....... Desired right wheel velocity.
--
-- Signal outputs:
-- p ..........	Current position.
-- h .......... Current heading.
-- v ..........	Current translational velocity.

simbotDynamics' ::
    Length -> Acceleration -> Speed -> Position2 -> Heading -> Velocity
    -> SimbotDynamics'
simbotDynamics' d a_max ws_max p_0 h_0 v_0 = proc (v_ld, v_rd) -> do
    rec let v_ld' = symLimit ws_max v_ld
            v_rd' = symLimit ws_max v_rd
            a     = signum (v_rd' + v_ld' - 2 * v) * a_max
            v_l   = v - (v_rd' - v_ld') / 2
            v_r   = v + (v_rd' - v_ld') / 2
        v <- (v_0 +)   ^<< integral -< a
        h <- hdngIgrl h_0           -< (v_r - v_l) / d
        p <- (p_0 .+^) ^<< integral -< vector2Polar v h
    returnA -< (p, h, v) -- Note: h is normalized!
    where
        limit ll ul x = if x < ll then ll else if x > ul then ul else x
        symLimit l = let absl = abs l in limit (-absl) absl

	-- Ensures the heading remains normalized (and the integral bounded).
	hdngIgrl :: Heading -> SF RotVel Heading
	hdngIgrl h_0 =
	    switch
		(proc rv -> do
		    h <- (h_0 +) ^<< integral -< rv
		    e <- edge -< h < (-pi) || h >= pi
		    returnA -< (h, e `tag` normalizeHeading h))
		hdngIgrl
	

------------------------------------------------------------------------------
-- Ball dynamics
------------------------------------------------------------------------------

{-
General structure
-----------------

Simple physical model with friction that slows a ball down and where
collisions are represented by (dirac) impulses. modelled as Events, which,
since all balls are assumed to be equally heavy and essentially weigthless
w.r.t. everything else carries instantaneous velocity changes.
-}

type BallDynamics = SF (Event Velocity2) (Position2, Velocity2)

-- Physical dynamics for a simple ball.
-- Arguments:
-- fc .........	Friction coefficient (constant).
-- dc ......... Aerodynamic drag coefficient (drag is prop. to v^2).
-- p_0 ........	Initial position.
-- v_0 ........	Initial velocity.
--
-- Signal inputs:
-- iv .........	Velocity impulses due to collisions: causes instantaneous
--		change in velocity.
--
-- Signal outputs:
-- p ..........	Current position.
-- v ..........	Current velocity.

ballDynamics ::
    YFrobReal -> YFrobReal -> Position2 -> Velocity2 -> BallDynamics
ballDynamics fc dc p_0 v_0 = proc iv -> do 
    rec let nv = norm v
            a  = if nv > 0 then
		     (fc/nv + dc*nv) *^ negateVector v
                 else
		     zeroVector
        v <- (v_0 ^+^) ^<< impulseIntegral -< (a, iv)
        p <- (p_0 .+^) ^<< integral -< v
    returnA -< (p, v)


------------------------------------------------------------------------------
-- Collisions
------------------------------------------------------------------------------

{-
1-dimensional Collisions
------------------------

Given an object with mass m1 moving at velocity v1 and an object with mass
m2 moving at velocity v2, assume that the collide fully elastiacally and
instantaneously. Then the following hold in general for the velocities
v1' and v2' after the collision:

    v1' = (m1 * v1 + m2 * v2 +/- m2 * abs (v1 - v2)) / (m1 + m2)
or
    v1' - v1 = m2 * (v2 - v1 +/- abs (v1 - v2)) / (m1 + m2)

and
    v2' = (m1 * v2 + m2 * v2 -/+ m1 * abs (v1 - v2)) / (m1 + m2)
or
    v2' - v2 = m1 * (v1 - v2 -/+ abs (v1 - v2)) / (m1 + m2)

The signs are given by geometrical constraints, i.e. assuing that the
objects cannot move through each other.

Given the assumptions that all balls weigh the same and that balls are much
lighter than everything else, we get two special cases.

1. Two balls collide, i.e. m1 = m2 = m.

2. A ball and a heavy object (or wall) collide. E.g. m1 >> m2.

For case 1 we get:

    v1' = (v1 + v2 +/- abs (v1 - v2)) / 2
or
    v1' - v1 = (v2 - v1 +/- abs (v1 - v2)) / 2

and
    v2' = (v2 + v2 -/+ abs (v1 - v2)) / 2
or
    v2' - v2 = (v1 - v2 -/+ abs (v1 - v2)) / 2

For case 2 we get:

    v1' ~= v1
or
    v1' - v1 = 0
and

    v2' ~= v1 + abs (v1 - v2)
or
    v1' - v1 = v1 - v2 +/- abs (v1 - v2)

-}

-- Impulses are modelled as events.
-- !!! This currently constitute a breach of abstraction!
impulse :: VectorSpace v k => v -> Event v
impulse v = Event v


noImpulse :: VectorSpace v k => Event v
noImpulse = noEvent


-- Check if an animate object has hit any of the fixed walls.
-- All animate objects are currently considered to be circles as far as
-- modelling of collisions go.
hitFixedWall :: Object -> Event Velocity2
hitFixedWall obj =
    if (x - r < worldWestWall && vx < 0)
       || (x + r > worldEastWall && vx > 0) then
	impulse (vector2 (-(2 * vx)) 0)
    else if (y - r < worldSouthWall && vy < 0)
	    || (y + r > worldNorthWall && vy > 0) then
	impulse (vector2 0 (-(2 * vy)))
    else
	noImpulse
    where
        Point2 x y = objPos obj
        r          = objRadius obj
        (vx, vy)   = vector2XY (objVel obj)


-- Check if animate object has hit another object.
-- Animate objects are assumed to be round, inanimate objects are assumed
-- to be rectangular and fixed (infinitely heavy).
hit :: Object -> Object -> Event Velocity2
obj1 `hit` obj2 | objBBox obj1 `intersects` objBBox obj2 = hit'
                | otherwise                              = noImpulse
    where
	hit' = if obj2 `inClass` ClsInanimate then
		   hitInanimate p r v obj2
               else
	           case obj1 of
		       ObjSimbotA {} -> robotHit p r v obj2
		       ObjSimbotB {} -> robotHit p r v obj2
                       ObjBall {}    -> ballHit p r v obj2
                       _             -> intErrObjPhys "hit"
                                                      "Unknown animate object"
        p = objPos obj1
        r = objRadius obj1
        v = objVel obj1


-- Check for animate object (round) hitting inanimate (rectangular) object.
hitInanimate :: Position2 -> Length -> Velocity2 -> Object -> Event Velocity2
hitInanimate p r v obj
    -- Hit straight from left or right (west/east)?
    | x < x1 && y1 < y && y < y2 && x + r > x1 && vx > 0
      || x > x2 && y1 < y && y < y2 && x - r < x2 && vx < 0
	= impulse (vector2 (-(2 * vx)) 0)
    -- Hit straight from top or bottom (nort/south)?
    | x1 < x && x < x2 && y < y1 && y + r > y1 && vy > 0
      || x1 < x && x < x2 && y > y2 && y - r < y2 && vy < 0
	= impulse (vector2 0 (-(2 * vy)))
    -- Hit lower left corner?
    | x <= x1 && y <= y1
	= hitInanimateCorner r (p11 .-. p) v
    -- Hit upper left corner?
    | x <= x1 && y >= y2
	= hitInanimateCorner r (p12 .-. p) v
    -- Hit upper right corner?
    | x >= x2 && y >= y2
	= hitInanimateCorner r (p22 .-. p) v
    -- Hit lower right corner?
    | x >= x2 && y <= y1
	= hitInanimateCorner r (p21 .-. p) v
    | otherwise
	= noImpulse
    where
	Point2 x y = p
        (vx, vy) = vector2XY v
        BBox p11@(Point2 x1 y1) p22@(Point2 x2 y2) = objBBox obj
        p12 = Point2 x1 y2
        p21 = Point2 x2 y1

	hitInanimateCorner r d v
	    -- Closer than r and approaching?
	    | nd < r && v_approach > 0
		= impulse ((-(2*v_approach)) *^ d_hat)
	    | otherwise
		= noImpulse
	    where
		nd = norm d
		d_hat = d ^/ nd
		v_approach = v `dot` d_hat     
	

-- Check for robot hitting animate object.	
robotHit :: Position2 -> Length -> Velocity2 -> Object -> Event Velocity2
robotHit _ _ _ (ObjBall {}) = noImpulse	-- Robots are not affected by balls.
robotHit p r v robot
    | nd < r + objRadius robot && v_approach > 0
	-- Robots are equally heavy.
        = impulse ((-v_approach) *^ d_hat)
    | otherwise
	= noImpulse
    where
	d = objPos robot .-. p
        nd = norm d
        d_hat = d ^/ nd
        v_rel = v ^-^ objVel robot
        v_approach = v_rel `dot` d_hat


-- Check for ball hitting animate object.
ballHit :: Position2 -> Length -> Velocity2 -> Object -> Event Velocity2
ballHit p r v obj
    | nd < r + objRadius obj && v_approach > 0
	= if obj `inClass` ClsBall then
	      -- Balls are equally heavy.
              impulse ((-v_approach) *^ d_hat)
          else
	      -- Robots are infinitely heavy compared to balls.
              impulse (((-2) * v_approach) *^ d_hat)
    | otherwise
	= noImpulse
    where
	d = objPos obj .-. p
        nd = norm d
        d_hat = d ^/ nd
        v_rel = v ^-^ objVel obj
        v_approach = v_rel `dot` d_hat

	
{-
    x11 <= x22 && x12 >= x21
    && y11 <= y22 && y12 >= y21
    && approaching
    where
	BBox (Point2 x11 y11) (Point2 x12 y12) = objBBox obj1
	BBox (Point2 x21 y21) (Point2 x22 y22) = objBBox obj2
        approaching = ((objVel obj2 ^-^ objVel obj1)
		       `dot` (objPos obj2 .-. objPos obj1))
		      < 0
-}

------------------------------------------------------------------------------
-- Sonar
------------------------------------------------------------------------------

-- Sonar echo from fixed walls. Currently simplified, more like laser ranger.
-- Arguments:
-- p ..........	Robot position.
-- r ..........	Robot radius. Sonar devices assumed to be mounted on perimeter.
-- h ..........	Heading for sonar device.
-- phi ........	Lobe width.
-- smr ........	Maximal sonar range.
--
-- Returns: distance to wall in given direction.

-- !!! Could make this better by also computing distances for h +/- (phi/2).

sonarEchoFixedWall ::
    Position2 -> Length -> Heading -> Angle -> Distance
sonarEchoFixedWall p r h phi =
    distanceFixedWall p (normalizeHeading h) - r
    where
	-- Distance to fixed wall in given direction. The position is assumed
	-- to be inside the fixed walls, the heading is assumed to be
	-- normalized.
	distanceFixedWall :: Position2 -> Heading -> Distance
	distanceFixedWall p h = wd
	    where
		Point2 x y = p
	
		-- East wall
		wd1 = if (-pi/2) < h && h < pi/2 then
			  ((worldEastWall - x) / cos h)
		      else
			  1.0e100 -- Hack! Works as long as world not too big.
	
		-- North wall	
		wd2 = if 0 < h && h < pi then
			  min wd1 ((worldNorthWall - y) / sin h)
		      else
			 wd1
	
		-- West wall
		wd3 = if h < (-pi/2) || h > pi/2 then
			  min wd2 ((worldWestWall - x) / cos h)
		      else
			 wd2
	
		-- South wall
		wd  = if (-pi) < h && h < 0 then
			  min wd3 ((worldSouthWall - y) / sin h)
		      else
			 wd3
	

-- Sonar echo for an object. Currently very simplified: object is treated
-- as if it did not have any physical size. Some form of projection for
-- computing the visible surface area and scaling to take the distance into
-- account would be a good idea. But then the interface may need to change.
-- Sonar eche from fixed walls. Currently simplified, more like laser ranger.
-- Arguments:
-- p ..........	Robot position.
-- h ..........	Robot heading.
-- rr..........	Robot radius. Sonar devices assumed to be mounted on perimeter.
-- phi ........	Lobe width.
-- obj ........	Object to compute echo for.
--
-- Returns: angle relative own heading (not normalized) and distance to
-- object.

sonarEcho :: Position2 -> Heading -> Length -> Angle -> Object
             -> (Angle, Distance)
sonarEcho p h rr phi obj = (h' - h, rho - rr)
    where
	(rho, h') = vector2RhoTheta (objPos obj .-. p)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

intErrObjPhys :: String -> String -> a
intErrObjPhys = intErr "RobotSim.ObjectPhysics"
