{-# LANGUAGE Arrows #-}

module AFPDemos where

import FRP.YFrob.RobotSim

----------------------------------------------------------------------

-- Template illustrating how to select specific controller based on the
-- robot identity.

rcA :: SimbotController
rcA rProps =
  case rpId rProps of
    1 -> rcA1 rProps
    2 -> rcA2 rProps
    3 -> rcA3 rProps

rcA1, rcA2, rcA3 :: SimbotController
rcA1 = undefined
rcA2 = undefined
rcA3 = undefined

----------------------------------------------------------------------

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
    in constant (mrFinalize $ ddVelTR vel rMax)

rcReverse :: Velocity -> SimbotController
rcReverse v rps = beh `dSwitch` const (rcReverse (-v) rps)
  where beh = proc sbi -> do
                stuckE <- rsStuck -< sbi
                let mr = ddVelDiff v v `mrMerge` 
                           tcoPrintMessage (tag stuckE "Ouch!!")
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
        vel' = lim vMax vel
        k    = 2
    in proc sbi -> do
         let phi = normalizeAngle (hd - odometryHeading sbi)
         let vel'' = (1 - abs phi / pi) * vel'
         returnA -< mrFinalize (ddVelTR vel'' (k*phi))

rcHeading' :: Velocity -> Heading -> SimbotController
rcHeading' vel hd rps = 
  proc sbi -> do
    rcHeadingAux rps -< (sbi, vel, hd)

rcHeadingAux :: SimbotProperties -> 
                SF (SimbotInput,Velocity,Heading) SimbotOutput
rcHeadingAux rps = 
    let vMax = rpWSMax rps
        k    = 2
    in proc (sbi,vel,hd) -> do
         let vel' = lim vMax vel
         let phi = normalizeAngle (hd - odometryHeading sbi)
         let vel'' = (1 - abs phi / pi) * vel'
         returnA -< mrFinalize (ddVelTR vel'' (k*phi))

rcMoveTo :: Velocity -> Position2 -> SimbotController
rcMoveTo vd pd rps = proc sbi -> do
    let (d,h) = vector2RhoTheta (pd .-. odometryPosition sbi)
        vel   = if d>2 then vd else vd*(d/2)
    rcHeadingAux rps -< (sbi, vel, h)

rcGoToBall :: Velocity -> SimbotController
rcGoToBall vd rps = proc sbi -> do
    let (phi, d) = head (aotBalls sbi ++ [(0.0,0.0)])
        h        = odometryHeading sbi
    rcHeadingAux rps -< (sbi, vd, h + phi)

rcGoToBall2 :: Velocity -> SimbotController
rcGoToBall2 vd rps =
    let loop = switch (rcGoToBall vd rps &&& rsStuck) $ \_ ->
               switch (constant (mrFinalize (ddVelTR (-vd) 0.3))
                       &&& after 2.5 ()) $ \_ ->
               loop
    in
        loop


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
  let neighbors = aotOtherRobots sbi
      vs = map (\(_,_,a,d) -> vector2Polar d a) neighbors
      avg = if vs==[] then zeroVector
            else foldl1 (^+^) vs ^/ fromInteger (toInteger (length vs))
      heading = vector2Theta avg + odometryHeading sbi
  rcHeadingAux rps -< (sbi, v, heading)
  -- o <- rcHeadingAux' rps -< (sbi, v, heading)
  -- printE <- repeatedly 1.0 () -< ()
  -- returnA -< mrFinalize (mrMerge o
  --               (tcoPrintMessage (tag printE (show (vs,heading)))))

