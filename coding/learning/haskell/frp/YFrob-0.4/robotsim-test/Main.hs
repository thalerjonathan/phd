{-# LANGUAGE Arrows #-}

module Main where

import Data.List (sortBy)

import FRP.YFrob.RobotSim

{-
-- Robot B tends to get stuck. Why???
world :: WorldTemplate
world = [ OTSimbotA {otRId = 1, otPos = Point2 (-2) (-2), otHdng=pi/4},
          OTSimbotA {otRId = 2, otPos = Point2 (-2) 0, otHdng=0},
          OTSimbotA {otRId = 3, otPos = Point2 (-2) 2, otHdng=(-pi/4)},
          OTSimbotA {otRId = 4, otPos = Point2 (-3) 1, otHdng=pi/8},
	  -- OTSimbotB {otRId = 1, otPos = Point2 3 (-4), otHdng=(pi/2)},
	  OTSimbotB {otRId = 2, otPos = Point2 1 1, otHdng=0},
          OTBlock {otPos = Point2 (-4) 2},
          OTNSWall {otPos = Point2 (-4) 4},
          OTEWWall {otPos = Point2 (-2) (-2)},
          OTVWall {otPos = Point2 (-3) 0},
          OTHWall {otPos = Point2 3 2},
	  OTBall {otPos = Point2 3 (-2)},
          OTBall {otPos = Point2 2 2}
	  ]


main = runSim (Just world) (rcReverse 0.5) (rcReverse2 1.0)  -- (rcAlign 0.0) (rcAlign 0.0) -- sc1 (rcReverse 1.0)
-}

world :: WorldTemplate
world = [ OTSimbotA {otRId = 1, otPos = Point2 4.5 3.5, otHdng=0},
	  OTSimbotB {otRId = 2, otPos = Point2 2.5 1.5, otHdng=0},
	  OTSimbotB {otRId = 3, otPos = Point2 3 3.5, otHdng=0},
          OTBlock {otPos = Point2 (4.5) 2},
          OTNSWall {otPos = Point2 (4) 3.5},
          OTEWWall {otPos = Point2 (3.5) (1)}
        ]


main = runSim (Just world) sc1 rcStop


sc1 :: SimbotController
sc1 sp = proc si -> do
    e <- repeatedly 0.5 () -< ()
    let etco = e `tag` show (rfLeft si)
    returnA -< mrFinalize (ddVelTR 0.0 0.2 `mrMerge` tcoPrintMessage etco)


sc2 :: SimbotController
sc2 sp = proc _ -> do
    returnA -< mrFinalize (ddVelTR 0.5 0.5)


sc3 :: SimbotController
sc3 sp = gotoXYs tps 0.90
    where
	tps = [Point2 (-4) (-4), Point2 0 4, Point2 4 (-4)]

sc4 :: SimbotController
sc4 sp = gotoXYs (cycle tps) 2.0
    where
	tps = [Point2 3 3, Point2 (-3) 3, Point2 (-3) (-3), Point2 3 (-3)]


gotoXYs :: [Position2] -> Velocity -> SF SimbotInput SimbotOutput
gotoXYs []         _ = stop
gotoXYs (tp : tps) v = switch (gotoXY tp v) $ \_ ->
                       gotoXYs tps v


gotoXY :: Position2 -> Velocity -> SF SimbotInput (SimbotOutput, Event ())
gotoXY tp v = towardsTarget &&& closeEnough
    where
        towardsTarget = proc si -> do
	    let tv       = tp .-. odometryPosition si
		(td, th) = (vector2RhoTheta tv)
		dh       = normalizeHeading (th - odometryHeading si)
		-- Eager to turn towards goal.
		v_l      = cos dh * (min (td^2) v) - sin (dh / 2) * v
		v_r      = cos dh * (min (td^2) v) + sin (dh / 2) * v
		-- Reluctant to turn when facing 180° from goal dir.
		-- v_l   = cos dh * (min (td^2) v) - sin dh * v
		-- v_r   = cos dh * (min (td^2) v) + sin dh * v
	    returnA -< mrFinalize (ddVelDiff v_l v_r)

        closeEnough = proc si -> do
            let td = norm (tp .-. odometryPosition si) < 0.05
            e <- iEdge False -< td
	    returnA -< e


stop :: SF a SimbotOutput
stop = constant (mrFinalize ddBrake)


------------------------------------------------------------------------------
-- Paul's controllers
------------------------------------------------------------------------------

rcStop :: SimbotController
rcStop _ = constant (mrFinalize ddBrake)

rcBlind1 _ = constant (mrFinalize $ ddVelDiff 10 10)

rcBlind2 rps = 
    let max = rpWSMax rps
    in constant (mrFinalize $ ddVelDiff (max/2) (max/2))

rcTurn :: Velocity -> SimbotController
rcTurn vel rps = 
    let vMax = rpWSMax rps
        rMax = 2 * (vMax - vel) / rpDiameter rps
    in constant (mrFinalize $ ddVelTR (vMax/2) rMax)

rcReverse :: Velocity -> SimbotController
rcReverse v rps = beh `dSwitch` const (rcReverse (-v) rps)
  where beh = proc sbi -> do
              stuckE <- rsStuck -< sbi
              let mr = ddVelDiff v v
                       `mrMerge` tcoPrintMessage (tag stuckE "Ouch!!")
              returnA -< (mrFinalize mr, stuckE)

rcReverse2 :: Velocity -> SimbotController
rcReverse2 v rps = beh `dSwitch` const (rcReverse2 (-v) rps)
  where beh = proc sbi -> do
              stuckE <- rsStuck -< sbi
              e <- repeatedly 1.0 () -< ()
              st <- systemTime -< sbi	      
              let mr = ddVelDiff v v
                       `mrMerge` tcoPrintMessage (tag stuckE "Ouch!!")
                       `mrMerge` tcoPrintMessage (tag e (show st))
              returnA -< (mrFinalize mr, stuckE)

rcReverse' v rps = 
  (rsStuck >>> arr fun) `dSwitch` const (rcReverse' (-v) rps)
  where fun stuckE = 
            let mr = ddVelDiff v v `mrMerge` 
                       tcoPrintMessage (tag stuckE "Ouch!!")
            in (mrFinalize mr, stuckE)

rcHeading :: Velocity -> Heading -> SimbotController
rcHeading vel hd rps = 
    let vMax = rpWSMax rps
        rMax = 2 * (vMax - vel) / rpDiameter rps
    in proc sbi -> do
       let he  = normalizeAngle (hd - odometryHeading sbi)
           rot = if he < 0 -- (pi<he) || ((-pi<he) && (he<0)) 
                 then -rMax else rMax
       returnA -< mrFinalize (ddVelTR (vMax/2) {- he -} rot)

rcHeading1 :: Velocity -> Heading -> SimbotController
rcHeading1 vel hd rps = 
    let vMax = rpWSMax rps
        rMax = 2 * (vMax - vel) / rpDiameter rps
    in proc sbi -> do
       let ang = normalizeAngle (hd - odometryHeading sbi)
           rot = if ang < 0 then -rMax else rMax
       returnA -< mrFinalize (ddVelTR (vMax/2) {- he -} rot)

rcHeading2 :: Velocity -> Heading -> SimbotController
rcHeading2 vel hd rps = 
    let vMax = rpWSMax rps
        rMax = 2 * (vMax - vel) / rpDiameter rps
    in proc sbi -> do
       let ang = normalizeAngle (hd - odometryHeading sbi)
       returnA -< mrFinalize (ddVelTR (vMax/2) (2 * ang))

rcHeading' :: Velocity -> Heading -> SimbotController
rcHeading' vel hd rps = 
  proc sbi -> do
    rcHeadingAux rps -< (sbi, vel, hd)

rcHeading'' :: Velocity -> Heading -> SimbotController
rcHeading'' vel hd rps = 
  proc sbi -> do
    t <- localTime -< ()
    rcHeadingAux rps -< (sbi, vel, hd + sin(2 * pi * t))


rcHeadingAux :: SimbotProperties -> 
               SF (SimbotInput,Velocity,Heading) SimbotOutput
rcHeadingAux rps = 
    let vMax = rpWSMax rps
    in proc (sbi,vel,hd) -> do
       let rMax = 2 * (vMax - vel) / rpDiameter rps
           he   = hd - odometryHeading sbi
           rot  = if (pi<he) || ((-pi<he) && (he<0)) 
                  then -rMax else rMax
       returnA -< mrFinalize (ddVelTR (vMax/2) rot)

rcMoveTo :: Velocity -> Position2 -> SimbotController
rcMoveTo vd pd rps = proc sbi -> do
    let (d,h) = vector2RhoTheta (pd .-. odometryPosition sbi)
        vel   = if d>2 then vd else vd*(d/2)
    rcHeadingAux rps -< (sbi, vel, h)

lim m y = max (-m) (min m y)

rcFollowLeftWall :: Velocity -> Distance -> SimbotController
rcFollowLeftWall v d _ = proc sbi -> do
  let r = rfLeft sbi
  dr <- derivative -< r
  let omega = kp*(r-d) + kd*dr
      kd    = 5
      kp    = v*(kd^2)/4
  returnA -< mrFinalize (ddVelTR v (lim 0.2 omega))

rcAlign :: Velocity -> SimbotController
rcAlign v rps = proc sbi -> do
  let neighbors = sortBy (\(_,_,_,d1) (_,_,_,d2) -> compare d1 d2) 
                         (aotOtherRobots sbi)
      (l1:l2:l3:_) = map (\(_,_,a,d) -> vector2Polar d a) neighbors
      vhat = vector2Polar v (odometryHeading sbi)
      heading l = vector2Theta (vhat ^+^ l)
  dl1 <- derivative -< l1
  dl2 <- derivative -< l2
  dl3 <- derivative -< l3
  let hAvg   = sum (map heading [dl1,dl2,dl3]) / 3
  rcHeadingAux rps -< (sbi, v, hAvg)
