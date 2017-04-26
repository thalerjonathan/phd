{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		ObjectTemplate					     *
*       Purpose:	Interface types and functions for creating objects   *
*			and worlds.					     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim.ObjectTemplate (
    WorldTemplate,
    ObjectTemplate(..),
    objectToOT,		-- :: Object -> ObjectTemplate
    BoundingBox,
    boundingBox		-- :: ObjectTemplate -> BoundingBox
) where

import FRP.YFrob.Common.PhysicalDimensions
import FRP.YFrob.Common.RobotIO (RobotId)

import FRP.YFrob.RobotSim.Object (Object(..))


type WorldTemplate = [ObjectTemplate]


-- !!! Maybe objects ought to be parameterized to a larger extent.
-- !!! E.g. a single robot type parameterized on shape (enumeration type
-- !!! used both in ObjectTemplate and in Object), colour, size, inertia,
-- !!! and so on. Similarly, walls and blocks could be instances of a single
-- !!! object parameterized on shape and colour.

data ObjectTemplate =
      OTBlock {			-- Square-shaped obstacle
	  otPos  :: Position2
      }
    | OTNSWall {		-- North-South wall segment
	  otPos  :: Position2
      }
    | OTEWWall {		-- East-west wall segment
	  otPos  :: Position2
      }
    | OTVWall {			-- "Vertical" wall segment, same as North-South
	  otPos  :: Position2
      }
    | OTHWall {			-- "Horizontal" wall segment, same as east-west
	  otPos  :: Position2
      }
    | OTSimbotA {		-- Robot of type Simbot A
          otRId  :: RobotId,
	  otPos  :: Position2,
	  otHdng :: Heading
      }
    | OTSimbotB {		-- Robot of type Simbot B
          otRId  :: RobotId,
	  otPos  :: Position2,
	  otHdng :: Heading
      }
    | OTBall {			-- Ball
	  otPos  :: Position2
      }


objectToOT :: Object -> ObjectTemplate
objectToOT (ObjBlock  {objPos = p}) = OTBlock  {otPos = p}
objectToOT (ObjNSWall {objPos = p}) = OTNSWall {otPos = p}
objectToOT (ObjEWWall {objPos = p}) = OTEWWall {otPos = p}
objectToOT (ObjSimbotA {objRId = rid, objPos = p, objHdng = h}) =
    OTSimbotA {otRId = rid, otPos = p, otHdng = h}
objectToOT (ObjSimbotB {objRId = rid, objPos = p, objHdng = h}) =
    OTSimbotB {otRId = rid, otPos = p, otHdng = h}
objectToOT (ObjBall {objPos = p}) = OTBall {otPos = p}


type BoundingBox = (Position2, Position2)

-- This is probably not such a great way of finding out the size of objects?
boundingBox :: ObjectTemplate -> BoundingBox
boundingBox = undefined


{-
-- This should be redone.
-- Define a simple textual format for the world (which equally well could be
-- created in a text editor) and write a parser/printer for that.
-- For example, one object per line, attributes separated by spaces.

data ObjectForShowRead = ObjectForShowRead
  { osrType :: String,
    osrId   :: ObjId,
    --osrSel  :: Bool,
    osrPos  :: Point2,
    --osrBBox :: BBox,
    osrHdng :: Maybe Heading
  }
  deriving (Show, Read)

objToObjSR :: Object -> ObjectForShowRead
objToObjSR obj =
  ObjectForShowRead {
    osrType = typeOf obj,
    osrId   = oId    obj,
    --osrSel  = oSel   obj,
    osrPos  = oPos   obj,
    --osrBBox = oBBox  obj,
    osrHdng = hdngOf obj
  } where
      typeOf (ObjBlock   {}) = "Block"
      typeOf (ObjNSWall  {}) = "NSWall"
      typeOf (ObjEWWall  {}) = "EWWall"
      typeOf (ObjSimbotA {}) = "SimbotA"
      typeOf (ObjSimbotB {}) = "SimbotB"
      typeOf _               = intErr "RSObjectTemplate"
                                      "typeOf"
                                       "unknown object type"

      hdngOf (ObjSimbotA { orHdng = hdng }) = Just hdng
      hdngOf (ObjSimbotB { orHdng = hdng }) = Just hdng
      hdngOf _                              = Nothing

objSRToObj :: ObjectForShowRead -> Object
objSRToObj osr = obj where
  obj = initObj {
    oId   = osrId   osr,
    oSel  = False, -- osrSel  osr,
    oPos  = osrPos  osr,
    oBBox = objBBox obj -- osrBBox osr
  }

  initObj = case osrType osr of
    "Block"   -> ObjBlock {}
    "NSWall"  -> ObjNSWall {}
    "EWWall"  -> ObjEWWall {}
    "SimbotA" -> defaultSimbotA { orHdng = hdng }
    "SimbotB" -> defaultSimbotB { orHdng = hdng }

  Just hdng = osrHdng osr

instance Show Object where
  show obj = show (objToObjSR obj)

instance Read Object where
  readsPrec _ = readParen False $
		  \r -> [(objSRToObj osr, s) | (osr, s) <- reads r]
-}
