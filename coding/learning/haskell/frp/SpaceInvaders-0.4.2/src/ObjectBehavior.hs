{- $Id: ObjectBehavior.as,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:		ObjectBehavior					     *
*       Purpose:	Behavior of objects.				     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module ObjectBehavior (
    gun,	-- :: Position2 -> Object
    missile, 	-- :: Position2 -> Velocity2 -> Object
    alien 	-- :: RandomGen g => g -> Position2 -> Object
) where

import qualified System.Random as Random

import FRP.Yampa
import FRP.Yampa.Utilities
import FRP.Yampa.Geometry

import PhysicalDimensions
import WorldGeometry
import Parser
import Object


------------------------------------------------------------------------------
-- Gun
------------------------------------------------------------------------------

gun :: Position2 -> Object
gun (Point2 x0 y0) = proc (ObjInput {oiGameInput = gi}) -> do
    -- Position.
    (Point2 xd _) <- ptrPos -< gi		-- Desired position
    rec
        -- Controller.
        let ad = 10 * (xd - x) - 5 * v		-- Desired acceleration

        -- Physics with hard limits on acceleration and speed.
        v <- integral -< let a = symLimit gunAccMax ad
                         in
			     if (-gunSpeedMax) <= v && v <= gunSpeedMax
                                || v < (-gunSpeedMax) && a > 0
                                || v > gunSpeedMax && a < 0
                             then a
                             else 0
        x <- (x0+) ^<< integral -< v

    -- Fire mechanism and ammunition level.
    trigger       <- lbp             -< gi
    (level, fire) <- magazine 20 0.5 -< trigger

    returnA -< ObjOutput {
	           ooObsObjState = oosGun (Point2 x y0) (vector2 v 0) level,
		   ooKillReq     = noEvent,
                   ooSpawnReq    =
                       fire `tag` [missile (Point2 x (y0 + (gunHeight/2))) 
                                           (vector2 v missileInitialSpeed)]
               }


-- Ammunition magazine. Reloaded up to maximal
-- capacity at constant rate.
-- n ... Maximal and initial number of missiles.
-- f ..........	Reload rate.
-- input ......	Trigger.
-- output .....	Tuple:
--   #1: Current number of missiles in magazine.
--   #2: Missile fired event.
magazine :: 
  Int -> Frequency 
      -> SF (Event ()) (Int, Event ())
magazine n f = proc trigger -> do
  reload <- repeatedly (1/f) () -< ()
  (level,canFire) 
      <- accumHold (n,True) -< 
             (trigger `tag` dec)
             `lMerge` (reload `tag` inc)
  returnA -< (level, 
	      trigger `gate` canFire)
  where
    inc :: (Int,Bool) -> (Int, Bool)
    inc (l,_) | l < n = (l + 1, l > 0)
              | otherwise = (l, True)
    dec :: (Int,Bool) -> (Int, Bool)
    dec (l,_) | l > 0 = (l - 1, True)
              | otherwise = (l, False)

-- Ammunition magazine. Reloaded up to maximal capacity at constant rate.
-- n ..........	Maximal and initial number of missiles.
-- f ..........	Reload rate.
-- input ......	Trigger.
-- output .....	Tuple:
--		#1 ....	Current number of missiles in magazine.
--		#2 ....	Missile fired.

{-
 Henrik's original version, commented out for now:

magazine :: Int -> Frequency -> SF (Event ()) (Int, Event ())
magazine n f = proc trigger -> do
    reload       <- repeatedly (1/f) ()      -< ()
    -- We have a reverse application operator #, but for some reason arrowp
    -- chokes on (#).
    newLevelFire <- accumFilter (flip ($)) n -< (trigger `tag` dec)
                                                `lMerge` (reload `tag` inc)
    level        <- hold n              -< fmap fst newLevelFire
    returnA -< (level, filterE snd newLevelFire `tag` ())
    where
	-- inc, dec :: Int -> (Int, Maybe (Int, Bool))
	inc l | l < n     = (l + 1, Just (l + 1, False))
              | otherwise = (l, Nothing)
        dec l | l > 0     = (l - 1, Just (l - 1, True))
              | otherwise = (l, Nothing)

-}

------------------------------------------------------------------------------
-- Missile
------------------------------------------------------------------------------

-- Of course, this would be much better if we used the real impulse stuff:
-- No bogus iPre, for instance.
missile :: Position2 -> Velocity2 -> Object
missile p0 v0 = proc oi -> do
    rec
        -- Basic physics
        vp  <- iPre v0                     -< v
        ffi <- forceField                  -< (p, vp)
        v  <- (v0 ^+^) ^<< impulseIntegral -< (gravity, ffi)
	p  <- (p0 .+^) ^<< integral        -< v
    die <- after missileLifeSpan () -< ()
    returnA -< ObjOutput {
	           ooObsObjState = oosMissile p v,
		   ooKillReq     = oiHit oi `lMerge` die,
                   ooSpawnReq    = noEvent
               }


------------------------------------------------------------------------------
-- Alien
------------------------------------------------------------------------------

type ShieldLevel = Double


-- Alien behavior.
-- g ..........	Random generator.
-- p0 .........	Initial position.
-- vyd ........ Desired vertical speed.

alien :: RandomGen g => g -> Position2 -> Velocity -> Object
alien g p0 vyd = proc oi -> do
    rec

        -- About 4% of time spent here.
	-- Pick a desired horizontal position.
        rx     <- noiseR (worldXMin, worldXMax) g -< () 
        sample <- occasionally g 5 ()             -< ()
        xd     <- hold (point2X p0)               -< sample `tag` rx    
	
        -- Controller. Control constants not optimized. Who says aliens know
	-- anything about control theory?
        let axd = 5 * (xd - point2X p) - 3 * (vector2X v)
	    ayd = 20 * (vyd - (vector2Y v))
	    ad  = vector2 axd ayd
	    h   = vector2Theta ad

        -- About 46% of time spent in Physics..
	-- Physics
	let a = vector2Polar (min alienAccMax (vector2Rho ad)) h
        vp  <- iPre v0                      -< v
        ffi <- forceField                   -< (p, vp)
        -- 28 % of time spent in the following line.
        v   <- (v0 ^+^) ^<< impulseIntegral -< (gravity ^+^ a, ffi)
        -- 25 % of time spent on the following line.
        -- (Surprising: integral should be cheaper than impulseIntegral,
        -- plus it ides not add up!)
	p   <- (p0 .+^) ^<< integral        -< v

	-- Shields
	sl  <- shield -< oiHit oi
	die <- edge   -< sl <= 0

    returnA -< ObjOutput {
	           ooObsObjState = oosAlien p h v,
		   ooKillReq     = die,
                   ooSpawnReq    = noEvent
               }
    where
	v0 = zeroVector



-- About 20% of the time spent here. 
shield :: SF (Event ()) ShieldLevel
shield = proc hit -> do
    rec
	let rechargeRate = if sl < slMax then slMax / 10 else 0
        sl <- (slMax +) ^<< impulseIntegral -< (rechargeRate, hit `tag` damage)
    returnA -< sl
    where
        slMax  = 100
        damage = -50


------------------------------------------------------------------------------
-- Force fields acting on objects
------------------------------------------------------------------------------

-- Object are subject to gravity and a strange repellent forcefield that
-- drives objects away from the edges, effectively creating a corridor.
-- The strange field is inversely proportional to the cube of the distance
-- from either edge. It is thought that the field is a remnant of a defence
-- system put in place by the mythical and technologically advanced
-- "Predecessors" eons ago.

{-
field :: Position2 -> Acceleration2
field (Point2 x _) = vector2 (leftAcc - rightAcc) 0 ^+^ gravity
    where
	leftAcc  = min (if x > worldXMin
                        then k / (x - worldXMin)^3
                        else maxAcc)
                       maxAcc
	rightAcc = min (if x < worldXMax
                        then k / (worldXMax - x)^3
                        else maxAcc)
                       maxAcc
        k        = 10000000
        maxAcc   = 10000
-}

-- New attempt. Force fields act like invisible walls.
-- The fact that this is a stateful *signal* function (Fields having state?
-- Come on ...), can be attributed to the fact that we are cheating in the
-- first place by abstracting events of short duration to instantaneous
-- events. "field" being a stateful signal functio is part of the price
-- one have to pay for that to make this work in practice.

-- Not much time spent here, it seems.

forceField :: SF (Position2, Velocity2) (Event Acceleration2)
forceField = proc (p, v) -> do
    lfi    <- edge    -< point2X p < worldXMin && vector2X v < 0
    rfi    <- edge    -< point2X p > worldXMax && vector2X v > 0
    returnA -< (mergeBy (^+^) (lfi `tag` (vector2 (-2 * vector2X v) 0))
                              (rfi `tag` (vector2 (-2 * vector2X v) 0)))

gravity = vector2 0 (-20)


------------------------------------------------------------------------------
-- Support
------------------------------------------------------------------------------

limit ll ul x = if x < ll then ll else if x > ul then ul else x

symLimit l = let absl = abs l in limit (-absl) absl
