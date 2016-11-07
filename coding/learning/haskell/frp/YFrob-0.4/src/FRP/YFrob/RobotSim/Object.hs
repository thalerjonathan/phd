{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		Object						     *
*       Purpose:	Definition of objects in the world and their static  *
*			properties.					     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- ToDo:
-- * Re-organize the object type.
--   - Single constructor for all robots probably a good start.
--   - Single constructor for all fixed objects too?
--   - Fields like robot id and robot type should be moved into
--     physical properties (stated purpose of RPP should be extended).

-- To think about:
-- Maybe "Object" is a misnomer, since the true objects are the signal
-- functions. Change to "ObjectState"? Or, since this is how it influences
-- the rest or the world, ObjectInfluence? ObjectOutput?

-- ToDo:
-- Rethink the object creation interface. Drag object templates into the
-- picture?

-- Note on names. For fields and other selectors, the "selector convention"
-- is used, i.e. using a type-based prefix. This is less natural when
-- one wants to emphasize an (possibly n-ary) operation, or a test
-- (predicate or relation), having a suggestive name such as "hit" or
-- "filter" or something. A prefix could still work, especially if it's
-- clear that it is a prefix (i.e. it is indecipherable!) and thus easily
-- can be classified as "noise" when reading. But when disambiguation is
-- needed, a suffix (i.e. the type name) may well be the better option.
-- Especially if the operation conceptually could be understood as
-- "overloaded". E.g. showXXX, filterXXX, ...

module FRP.YFrob.RobotSim.Object (
    Object(..),
    RobotPhysicalProperties(..),
    BallPhysicalProperties(..),
    BBox (..),
    objRadius,		-- :: Object -> Length
    block,		-- :: Bool -> Position2 -> Object
    nsWall,		-- :: Bool -> Position2 -> Object
    ewWall,		-- :: Bool -> Position2 -> Object
    simbotA,		-- :: RobotType -> RobotId -> Bool -> Position2
			--    -> Heading -> Velocity2 -> Object
    simbotB,		-- :: RobotType -> RobotId -> Bool -> Position2
                        --    -> Heading -> Velocity2 -> Object
    ball,		-- :: Bool -> Position2 -> Velocity2 -> Object
    objSetSel,		-- :: Object -> Bool -> Object
    objSetPos,		-- :: Object -> Position2 -> Object
    objSetPosRel,	-- :: Object -> Distance2 -> Object
    objSetHdng,		-- :: Object -> Heading -> Object
    touchesFixedWall,	-- :: Object -> Bool
    touches,		-- :: Object -> Object -> Bool
    intersects,		-- :: BoundingBox -> BoundingBox -> Bool
    within,		-- :: Position2 -> BoundingBox -> Bool
    blockSide,		-- :: Length
    nsWallXSide,	-- :: Length
    nsWallYSide,	-- :: Length
    ewWallXSide,	-- :: Length
    ewWallYSide,	-- :: Length
    simbotARType,	-- :: RobotType
    simbotADiam,	-- :: Length
    simbotARadius,	-- :: Length
    simbotAAccMax,	-- :: Acceleration
    simbotAWSMax,	-- :: Speed
    simbotARFN,		-- :: Int
    simbotARFMaxRange,  -- :: Distance
    simbotBRType,	-- :: RobotType
    simbotBDiam,	-- :: Length
    simbotBRadius,	-- :: Length
    simbotBAccMax,	-- :: Acceleration
    simbotBWSMax,	-- :: Speed
    simbotBRFN,		-- :: Int
    simbotBRFMaxRange,  -- :: Distance
    ballDiam,		-- :: Length  
    ballRadius,		-- :: Length
    ObjClass(..),
    (<:),		-- :: ObjClass -> ObjClass -> Bool
    objClass,		-- :: Object -> ObjClass
    inClass		-- :: Object -> ObjClass -> Bool
) where

import FRP.Yampa.Geometry
import FRP.Yampa.Forceable

import FRP.YFrob.Common.Diagnostics (intErr)
import FRP.YFrob.Common.PhysicalDimensions
import FRP.YFrob.Common.RobotIO (RobotId, RobotType)

import FRP.YFrob.RobotSim.WorldGeometry


------------------------------------------------------------------------------
-- Object type with constructors and selectors
------------------------------------------------------------------------------

-- Note: Objects should only be constructed/updated through the provided
-- constructors/update functions since some of the fields (e.g. oBBox)
-- are interdependent. The reason Object is not exported abstractly is that
-- it is convenient to inspect object by pattern matching.
-- 
-- To avoid space leaks, all fields of Object, *except* objBBox!, are strict.
-- Also, the special type BBox was introduced to ensure the bounding box was
-- completely evaluated once the enclosing object was evaluated.
--
-- !!! The Robot/Ball physical properties fields are currently rather
-- !!! pointless. The idea is that there in the furure will be a larger
-- !!! degree of parameterization, at least for robots and balls. E.g.
-- !!! balls with different masses, user-specifiable physical robot properties
-- !!! such as max velocity. Maybe even visual aspects such as colour. Or
-- !!! maybe even a rendering function could be part of the object
-- !!! representation.
--
-- !!! Maybe objects ought to be parameterized to a larger extent.
-- !!! E.g. a single robot type parameterized on shape (enumeration type
-- !!! used both in ObjectTemplate and in Object), colour, size, inertia,
-- !!! and so on. Similarly, walls and blocks could be instances of a single
-- !!! object parameterized on shape and colour. Or maybe the latter is
-- !!! fairly pointless, and only adds overhead? People are only interested
-- !!! in defining robots anyway.
--
-- !!! The fields objMsg and objPic has been removed. The current theory is
-- !!! that they don't belong in the object, although it would make the
-- !!! the output type for robot simulation signal function less verbose.
-- !!! But these fields are completely useless e.g. when editing, so the
-- !!! current choise is probably right. Maybe a message field would be
-- !!! a simple hack to enable robot to robot communication through
-- !!! their perception system, though?

data Object =
      ObjBlock {
          objSel  :: !Bool,
	  objPos  :: !Position2,
	  objBBox :: BBox		-- Not strict!
      }
    | ObjNSWall {
          objSel  :: !Bool,
	  objPos  :: !Position2,
	  objBBox :: BBox
      }
    | ObjEWWall {
          objSel  :: !Bool,
	  objPos  :: !Position2,
	  objBBox :: BBox
      }
    | ObjSimbotA {
          objRType :: !RobotType,
          objRId   :: !RobotId,
          objRPP   :: !RobotPhysicalProperties,
	  objSel   :: !Bool,
	  objPos   :: !Position2,
	  objHdng  :: !Heading,
          objVel   :: !Velocity2,
	  objBBox  :: BBox
      }
    | ObjSimbotB {
          objRType :: !RobotType,
          objRId   :: !RobotId,
          objRPP   :: !RobotPhysicalProperties,
	  objSel   :: !Bool,
	  objPos   :: !Position2,
	  objHdng  :: !Heading,
          objVel   :: !Velocity2,
	  objBBox  :: BBox
      }
    | ObjBall {
	  objBPP  :: !BallPhysicalProperties,
	  objSel  :: !Bool,
	  objPos  :: !Position2,
          objVel  :: !Velocity2,
	  objBBox :: BBox
      }


-- Properties pertaining to perception and physical interaction.
-- !!! Needs to include things like mass in order to do collisions etc.
-- !!! properly.
-- !!! Could also include function for computing (some) aspects of e.g. sonar
-- !!! echo, like perceived size and distance from certain direction.
data RobotPhysicalProperties = RPP {
    rppRadius	  :: !Length,	-- Robot radius.
    rppRFN	  :: !Int,	-- Number of range finders.
    rppRFMaxRange :: !Distance	-- Maximal range finder distance.
}


data BallPhysicalProperties = BPP {
    bppRadius	  :: !Length	-- Ball radius.
}


data BBox = BBox !Position2 !Position2
	    deriving Eq


objRadius :: Object -> Length
objRadius (ObjSimbotA {objRPP = RPP {rppRadius = r}}) = r
objRadius (ObjSimbotB {objRPP = RPP {rppRadius = r}}) = r
objRadius (ObjBall    {objBPP = BPP {bppRadius = r}}) = r
objRadius _ = intErrObj "objRadius" "Object does not have a radius."


------------------------------------------------------------------------------
-- Constants defining various object properties
------------------------------------------------------------------------------

-- Obstacles are square shaped.

blockSide :: Length
blockSide = 0.5


-- Walls are rectangular.

nsWallXSide, nsWallYSide :: Length
nsWallXSide = 0.1 
nsWallYSide = 1.0

ewWallXSide, ewWallYSide :: Length
ewWallXSide = 1.0
ewWallYSide = 0.1


-- Robots of type Simbot A are round.

simbotARType :: RobotType
simbotARType = "SimbotA"

simbotADiam, simbotARadius :: Length
simbotADiam   = 0.5
simbotARadius = simbotADiam / 2

simbotAAccMax :: Acceleration
simbotAAccMax = 0.2		-- Maximal translational acceleration.

simbotAWSMax :: Speed
simbotAWSMax = 1.0		-- Maximal (peripheral) wheel speed.

simbotARFN :: Int		-- Number of range finders.
simbotARFN = 8

simbotARFMaxRange :: Distance	-- Maximal range finder distance
simbotARFMaxRange = 2


-- Robots of type Simbot B currently behaves as a round robot too (but is drawn
-- triangular).

simbotBRType :: RobotType
simbotBRType = "SimbotB"

simbotBDiam, simbotBRadius :: Length
simbotBDiam   = 0.5
simbotBRadius = simbotBDiam / 2

simbotBAccMax :: Acceleration
simbotBAccMax = 0.5		-- Maximal translational acceleration.

simbotBWSMax :: Speed
simbotBWSMax = 2.0		-- Maximal (peripheral) wheel speed.

simbotBRFN :: Int		-- Number of range finders.
simbotBRFN = 8

simbotBRFMaxRange :: Distance	-- Maximal range finder distance
simbotBRFMaxRange = 2


-- The ball is, well, round

ballDiam, ballRadius :: Length
ballDiam   = 0.3
ballRadius = ballDiam / 2


------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

instance Forceable Object where
    force obj = objBBox obj `seq` obj


------------------------------------------------------------------------------
-- Smart constructors
------------------------------------------------------------------------------

block :: Bool -> Position2 -> Object
block sel p = obj
    where
	obj = ObjBlock {
		  objSel  = sel,
	          objPos  = p,
		  objBBox = computeObjBBox obj
              }


nsWall :: Bool -> Position2 -> Object
nsWall sel p = obj
    where
	obj = ObjNSWall {
		  objSel  = sel,
		  objPos  = p,
		  objBBox = computeObjBBox obj
	      }


ewWall :: Bool -> Position2 -> Object
ewWall sel p = obj
    where
	obj = ObjEWWall {
		  objSel  = sel,
		  objPos  = p,
		  objBBox = computeObjBBox obj
	      }


simbotA :: RobotType -> RobotId -> Bool -> Position2 -> Heading -> Velocity2
           -> Object
simbotA rtp rid sel p h v = obj
    where
	obj = ObjSimbotA {
                  objRType = rtp,
		  objRId   = rid,
                  objRPP   = rpp,
		  objSel   = sel,
		  objPos   = p,
		  objHdng  = normalizeHeading h,
                  objVel   = v,
		  objBBox  = computeObjBBox obj
	      }

	rpp = RPP {
		  rppRadius     = simbotARadius,
		  rppRFN        = simbotARFN,
		  rppRFMaxRange = simbotARFMaxRange
	      }


simbotB :: RobotType -> RobotId -> Bool -> Position2 -> Heading -> Velocity2
           ->Object
simbotB rtp rid sel p h v = obj
    where
	obj = ObjSimbotB {
                  objRType = rtp,
		  objRId   = rid,
                  objRPP   = rpp,
		  objSel   = sel,
		  objPos   = p,
		  objHdng  = normalizeHeading h,
                  objVel   = v,
		  objBBox  = computeObjBBox obj
	      }

	rpp = RPP {
		  rppRadius     = simbotBRadius,
		  rppRFN        = simbotBRFN,
		  rppRFMaxRange = simbotBRFMaxRange
              }


ball :: Bool -> Position2 -> Velocity2 -> Object
ball sel p v = obj
    where
	obj = ObjBall {
		  objBPP  = bpp,
		  objSel  = sel,
		  objPos  = p,
                  objVel  = v,
		  objBBox = computeObjBBox obj
	      }

	bpp = BPP {
		  bppRadius = ballRadius
              }


------------------------------------------------------------------------------
-- Object updating
------------------------------------------------------------------------------

objSetSel :: Object -> Bool -> Object
objSetSel obj sel = obj {objSel = sel}


objSetPos :: Object -> Position2 -> Object
objSetPos obj pos = obj'
    where
	obj' = obj {objPos = pos, objBBox = computeObjBBox obj'}


objSetPosRel :: Object -> Distance2 -> Object
objSetPosRel obj d = obj'
    where
	obj' = obj {objPos = objPos obj .+^ d, objBBox = computeObjBBox obj'}


objSetHdng :: Object -> Heading -> Object
objSetHdng obj h = obj'
    where
	-- The bounding box is affected if the shape of the robot is not round.
	obj' = obj {objHdng = normalizeHeading h, objBBox=computeObjBBox obj'}


------------------------------------------------------------------------------
-- Object and bounding box geometrical predicates
------------------------------------------------------------------------------

-- Check if object touches any of the fixed walls.
-- Note: The geometry of the current set of objects and of the current fixed
-- walls are such that a simple bounding box test is enough.

touchesFixedWall :: Object -> Bool
touchesFixedWall obj =
    x1 <= worldWestWall
    || x2 >= worldEastWall
    || y1 <= worldSouthWall
    || y2 >= worldNorthWall
    where
	BBox (Point2 x1 y1) (Point2 x2 y2) = objBBox obj


-- Check if two objects touch each other.
-- Currently only a bounding box test.
-- ToDo: if bounding box tests succeeds, then carry out precise test.

touches :: Object -> Object -> Bool
obj1 `touches` obj2 = (objBBox obj1) `intersects` (objBBox obj2)


intersects :: BBox -> BBox -> Bool
bb1 `intersects` bb2 =
    x11 <= x22 && x12 >= x21
    && y11 <= y22 && y12 >= y21
    where
	BBox (Point2 x11 y11) (Point2 x12 y12) = bb1
	BBox (Point2 x21 y21) (Point2 x22 y22) = bb2


within :: Position2 -> BBox -> Bool
(Point2 x y) `within` bb =
    x1 <= x && x <= x2 && y1 <= y && y <= y2
    where
	BBox (Point2 x1 y1) (Point2 x2 y2) = bb


------------------------------------------------------------------------------
-- Hierarchical object classification
------------------------------------------------------------------------------

-- Currently, all things which can move or be moved are considered to be
-- "animate". This seems convenient, even if it is not quite right.
-- We might want to have a more refined object hierarchy.
-- E.g. ClsAnimate for all "living" things (but currently only robots)
-- and ClsMovable for objects which can be moved (currently only balls).
-- Of course, the distinction between obstacles and movable objects might
-- be bad. A robot is also an obstacle, and so is a ball stuck between two
-- robots from the perspective of either robot!

data ObjClass =
      ClsObj		-- Top.
    | ClsInanimate	-- Superclass for all inanimate objects.
    | ClsBlock
    | ClsWall 		-- Superclass for all movable walls.
    | ClsNSWall 
    | ClsEWWall
    | ClsAnimate        -- Superclass for all animate objects.
    | ClsRobot		-- Superclass for all robot types.
    | ClsSimbotA
    | ClsSimbotB
    | ClsBall		-- Superclass for all balls. Currently only one type.
    deriving (Eq)


(<:) :: ObjClass -> ObjClass -> Bool
_            <: ClsObj       = True
ClsInanimate <: ClsInanimate = True
ClsBlock     <: ClsInanimate = True
ClsWall      <: ClsInanimate = True
ClsNSWall    <: ClsInanimate = True
ClsEWWall    <: ClsInanimate = True
ClsBlock     <: ClsBlock     = True
ClsWall      <: ClsWall      = True
ClsNSWall    <: ClsWall      = True
ClsEWWall    <: ClsWall      = True
ClsNSWall    <: ClsNSWall    = True
ClsEWWall    <: ClsEWWall    = True
ClsAnimate   <: ClsAnimate   = True
ClsRobot     <: ClsAnimate   = True
ClsSimbotA   <: ClsAnimate   = True
ClsSimbotB   <: ClsAnimate   = True
ClsBall      <: ClsAnimate   = True
ClsRobot     <: ClsRobot     = True
ClsSimbotA   <: ClsRobot     = True
ClsSimbotA   <: ClsSimbotA   = True
ClsSimbotB   <: ClsRobot     = True
ClsSimbotB   <: ClsSimbotB   = True
ClsBall      <: ClsBall      = True
_            <: _            = False


objClass :: Object -> ObjClass
objClass (ObjBlock {})   = ClsBlock
objClass (ObjNSWall {})  = ClsNSWall
objClass (ObjEWWall {})  = ClsEWWall
objClass (ObjSimbotA {}) = ClsSimbotA
objClass (ObjSimbotB {}) = ClsSimbotB
objClass (ObjBall {})    = ClsBall


inClass :: Object -> ObjClass -> Bool
obj `inClass` cls = objClass obj <: cls


------------------------------------------------------------------------------
-- Support functions
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

intErrObj :: String -> String -> a
intErrObj = intErr "RobotSim/Object"
