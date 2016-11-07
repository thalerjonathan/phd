{- $Id: Object.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:		Object						     *
*       Purpose:	Definition of objects in the world and their static  *
*			properties.					     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module Object (
    Object,
    ObjInput(..),
    ObjOutput(..),
    ObsObjState(..),
    oosGun,		-- :: Position2 -> Velocity2 -> ObsObjState
    oosMissile,		-- :: Position2 -> Velocity2 -> ObsObjState
    oosAlien,		-- :: Position2 -> Heading -> Velocity2 -> ObsObjState
    isGun,		-- :: ObsObjState -> Bool
    isMissile,		-- :: ObsObjState -> Bool
    isAlien,		-- :: ObsObjState -> Bool
    touches,		-- :: ObsObjState -> ObsObjState -> Bool
    approaches,		-- :: ObsObjState -> ObsObjState -> Bool
    colliding,		-- :: ObsObjState -> ObsObjState -> Bool
    gunRadius,		-- :: Length
    gunBase,		-- :: Length
    gunHeight,		-- :: Length
    gunSpeedMax,	-- :: Speed
    gunAccMax,		-- :: Acceleration
    missileRadius, 	-- :: Length
    missileInitialSpeed,-- :: Speed
    missileLifeSpan,	-- :: Time
    alienRadius,	-- :: Length
    alienWingRadius,	-- :: Length
    alienAccMax		-- :: Acceleration
) where

import FRP.Yampa (SF, Event)
import FRP.Yampa.Geometry
import FRP.Yampa.Forceable

import Parser (GameInput)
import PhysicalDimensions
import WorldGeometry


------------------------------------------------------------------------------
-- Object and related types
------------------------------------------------------------------------------

-- Objects are represented by signal functions, i.e. they are reactive and
-- can carry internal state.

type Object = SF ObjInput ObjOutput

data ObjInput = ObjInput {
    oiHit       :: Event (),
    oiGameInput :: GameInput
}

data ObjOutput = ObjOutput {
    ooObsObjState :: !ObsObjState,
    ooKillReq     :: Event (),
    ooSpawnReq    :: Event [Object]
}

-- Note: ObsObjState (Observable Object State) should only be constructed/
-- updated through the provided constructors/update functions since some of
-- the fields (e.g. if a bounding box field were added) might be dependent on
-- others. The reason ObsObjState is not exported abstractly is that it is
-- convenient to inspect it by pattern matching.
-- 
-- To avoid space leaks, all fields (except possibly dependent ones) are
-- strict.

data ObsObjState =
      OOSGun {
	  oosPos    :: !Position2,
	  oosVel    :: !Velocity2,
          oosRadius :: !Length,
          oosAmLvl  :: !Int
      }
    | OOSMissile {
	  oosPos    :: !Position2,
	  oosVel    :: !Velocity2,
          oosRadius :: !Length
      }
    | OOSAlien {
	  oosPos    :: !Position2,
	  oosHdng   :: !Heading,
	  oosVel    :: !Velocity2,
          oosRadius :: !Length
      }


------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

instance Forceable ObsObjState where
    -- If non-strict fields: oosNonStrict1 obj `seq` ... `seq` obj 
    force obj = obj


------------------------------------------------------------------------------
-- Smart constructors
------------------------------------------------------------------------------

-- If dependent fields (such as a bounding box) are added, these should also
-- be computed here.

oosGun :: Position2 -> Velocity2 -> Int -> ObsObjState
oosGun p v l = OOSGun {
                   oosPos    = p,
	           oosVel    = v,
	           oosRadius = gunRadius,
		   oosAmLvl  = l
               }


oosMissile :: Position2 -> Velocity2 -> ObsObjState
oosMissile p v = OOSMissile {
                     oosPos    = p,
	             oosVel    = v,
	             oosRadius = missileRadius
                 }


oosAlien :: Position2 -> Heading -> Velocity2 -> ObsObjState
oosAlien p h v = OOSAlien {
                     oosPos    = p,
	             oosHdng   = normalizeHeading h,
	             oosVel    = v,
	             oosRadius = alienRadius
                 }


------------------------------------------------------------------------------
-- Recognizers
------------------------------------------------------------------------------

isGun :: ObsObjState -> Bool
isGun (OOSGun {}) = True
isGun _           = False


isMissile :: ObsObjState -> Bool
isMissile (OOSMissile {}) = True
isMissile _               = False


isAlien :: ObsObjState -> Bool
isAlien (OOSAlien {}) = True
isAlien _             = False


------------------------------------------------------------------------------
-- Object and bounding box geometrical predicates
------------------------------------------------------------------------------


-- Check if two objects touch each other.
-- Currently only a radius-based intersection test.
touches :: ObsObjState -> ObsObjState -> Bool
oos1 `touches` oos2 =
    norm ((oosPos oos2) .-. (oosPos oos1)) < (oosRadius oos2 + oosRadius oos1)


-- Check if two objects are approaching each other.
approaches :: ObsObjState -> ObsObjState -> Bool
oos1 `approaches` oos2 =
    (oosVel oos2 ^-^ oosVel oos1) `dot` (oosPos oos2 .-. oosPos oos1) < 0.0


-- Check if two objects are colliding.
colliding :: ObsObjState -> ObsObjState -> Bool
oos1 `colliding` oos2 = oos1 `touches` oos2 && oos1 `approaches` oos2


------------------------------------------------------------------------------
-- Constants defining various object properties
------------------------------------------------------------------------------

-- All objects are treated as round as far as intersection tests etc. are
-- concerned, but they may be drawn using a different shape. It is assumed
-- that that shape is centered around the position of the object, and that
-- its size roughly corresponds to the size implied by the object radius.

-- Constants for gun.

-- Gun: drawn triangular
gunRadius, gunBase, gunHeight :: Length
gunRadius = 25
gunBase   = 2 * gunRadius
gunHeight = gunBase

gunSpeedMax :: Speed
gunSpeedMax = 500

gunAccMax :: Acceleration
gunAccMax = 500


-- Constants for missile.

-- Missiles: drawn circular.
missileRadius :: Length
missileRadius = 5.0

missileInitialSpeed :: Speed
missileInitialSpeed = 200

missileLifeSpan :: Time
missileLifeSpan = 5.0


-- Constants for alien.

-- Alien: drawn as a sphere with equatorial wing plane ("saturn-shaped")
alienRadius, alienWingRadius :: Length
alienRadius = 25
alienWingRadius = 2 * alienRadius

alienAccMax :: Acceleration
alienAccMax = 100


------------------------------------------------------------------------------
-- Support functions
------------------------------------------------------------------------------

{-
-- Old stuff

-- Bounding box is calculated once and cached inside object.

computeObjBBox :: Object -> BBox
computeObjBBox (ObjBlock {objPos = p}) = BBox (p .-^ d) (p .+^ d)
    where
        d = vector2 (blockSide / 2) (blockSide / 2)
computeObjBBox (ObjNSWall {objPos = p}) = BBox (p .-^ d) (p .+^ d)
    where
        d = vector2 (nsWallXSide / 2) (nsWallYSide / 2)
computeObjBBox (ObjEWWall {objPos = p}) = BBox (p .-^ d) (p .+^ d)
    where
        d = vector2 (ewWallXSide / 2) (ewWallYSide / 2)
computeObjBBox (ObjSimbotA {objPos = p}) = BBox (p .-^ d) (p .+^ d)
    where
        d = vector2 simbotARadius simbotARadius
computeObjBBox (ObjSimbotB {objPos = p, objHdng = d}) = BBox p1 p2
    where
        Point2 x1 y1 = p .+^ (vector2Polar simbotBRadius d) -- Nose
        Point2 x2 y2 = p .+^ (vector2Polar simbotBRadius (d + 2*pi/3))
        Point2 x3 y3 = p .+^ (vector2Polar simbotBRadius (d - 2*pi/3))

        p1 = Point2 (minimum [x1,x2,x3]) (minimum [y1,y2,y3])
        p2 = Point2 (maximum [x1,x2,x3]) (maximum [y1,y2,y3])
computeObjBBox (ObjBall {objPos = p}) = BBox (p .-^ d) (p .+^ d)
    where
        d = vector2 ballRadius ballRadius
-}
