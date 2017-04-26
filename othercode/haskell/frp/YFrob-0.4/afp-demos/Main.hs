module Main where

import System.Environment (getArgs)

import FRP.YFrob.RobotSim
import AFPDemos
import ITest


world :: WorldTemplate
world = [ OTSimbotA {otRId = 1, otPos = Point2 (-3) (-4), otHdng=(pi/2)},
	  OTSimbotB {otRId = 2, otPos = Point2 3 (-4), otHdng=(pi/2)}
	  ]

-- for rcTurn:
tworld :: WorldTemplate
tworld = [ OTSimbotA {otRId = 1, otPos = Point2 (-3) (-4), otHdng=(pi/2)},
	   OTSimbotB {otRId = 2, otPos = Point2 3 (-1), otHdng=(pi/2)}
	  ]

-- for blind and reverse tests:  A world with some obstacles:
world2 :: WorldTemplate
world2 = [ OTSimbotA {otRId = 1, otPos = Point2 (-2) (-2), otHdng=pi/4},
          OTSimbotA {otRId = 2, otPos = Point2 (-2) 0, otHdng=0},
          OTSimbotA {otRId = 3, otPos = Point2 (-2) 2, otHdng=(-pi/4)},
          OTSimbotA {otRId = 4, otPos = Point2 (-3) 1, otHdng=pi/8},
	  OTSimbotB {otRId = 1, otPos = Point2 3 (-4), otHdng=(pi/2)},
	  OTSimbotB {otRId = 2, otPos = Point2 1 1, otHdng=0},
          OTBlock {otPos = Point2 (-4) 2},
          OTNSWall {otPos = Point2 (-4) 4},
          OTEWWall {otPos = Point2 (-2) (-2)},
          OTVWall {otPos = Point2 (-3) 0},
          OTHWall {otPos = Point2 3 2},
	  OTBall {otPos = Point2 3.1 (-2)},
          OTBall {otPos = Point2 2 2}
	  ]

-- for align test:
world3 :: WorldTemplate
world3 = [ OTSimbotA {otRId = 1, otPos = Point2 (-4) (4), otHdng=pi/4},
          OTSimbotA {otRId = 2, otPos = Point2 (-3) 1, otHdng=pi/16},
          OTSimbotA {otRId = 3, otPos = Point2 (-2) (-2), otHdng=0},
          OTSimbotA {otRId = 4, otPos = Point2 0 0, otHdng=(-pi/4)}
          -- OTSimbotA {otRId = 3, otPos = Point2 (-2) 2, otHdng=(-pi/4)},
          -- OTSimbotA {otRId = 4, otPos = Point2 (-3) 1, otHdng=pi/8},
	  -- OTSimbotB {otRId = 3, otPos = Point2 3 (-4), otHdng=(pi/2)},
	  -- OTSimbotB {otRId = 4, otPos = Point2 1 1, otHdng=0}
	  ]

-- for wall following test:
world4 :: WorldTemplate
world4 = [ OTSimbotA {otRId = 1, otPos = Point2 (-4) (-3.3), otHdng=(pi/2)},
	  OTSimbotB {otRId = 2, otPos = Point2 3 (-3.5), otHdng=(pi)}
	  ]

afpDemos = [ {- ("rc",(Just world,rc,rc)), -}
	    ("rcStop",(Just world, rcStop, rcStop)),
	    ("rcBlind1",(Just world2, rcBlind1, rcBlind1)),
	    ("rcBlind2",(Just world2, rcBlind2, rcBlind2)),
	    ("rcTurn",(Just tworld, rcTurn 1, rcTurn 1.5)),
	    ("rcReverse",(Just world2, rcReverse 1, rcReverse 1)),
	    ("rcReverse'",(Just world2, rcReverse' 1, rcReverse' 1)),
	    ("rcHeading",(Just world, rcHeading 1 (pi/4), rcHeading 1 pi)),
	    ("rcHeading'",(Just world, rcHeading' 1 (pi/4), rcHeading' 1 pi)),
	    ("rcMoveTo",
	     (Just world,rcMoveTo 1 (Point2 3 4),rcMoveTo 1 (Point2 (-4) 4))),
	    ("rcGoToBall",(Just world2, rcReverse 1, rcGoToBall2 1)),
	    ("rcFollowLeftWall",
	     (Just world4,rcFollowLeftWall 0.5 0.5,rcFollowLeftWall 0.7 0.5)),
	    ("rcAlign",
	     (Just world3,rcAlign 1,rcAlign 1))
	    ]

ioDemos = map (\ (nm, (w,rc1,rc2)) -> (nm, runSim w rc1 rc2)) afpDemos

main :: IO ()
main =
  do args <- getArgs
     testShell ioDemos "AFP Demos> " args




