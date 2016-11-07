{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		Simulator					     *
*       Purpose:	The actual robot simulator			     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- !!! We are breaking the event abstraction here in order to facilitate
-- !!! programming of collision detectino and handling. One reason is that
-- !!! it currently relies on "dense events" in an attempt to ensure that
-- !!! robots don't get any where when a collision state persists.
-- !!! Another reason is that some of the routing functions for "par" need
-- !!! to compute events, and they are SCALAR functions. (One could potentially
-- !!! return something else and post-process it by pre-composing a signal
-- !!! function on each signal function in the collections, but this is
-- !!! undesirable for a number of reasons.)
-- !!! Maybe this indicates that event should not be abstract, or at least
-- !!! that there should be a constructor for Event. Or maybe it indicates
-- !!! that we do collisions in the wrong way. A reasonable argument *for*
-- !!! being able to construct events in scalar code is that they are a
-- !!! natural representation of impulses. Impulses may have to be computed
-- !!! by complicated interaction functions *within* a time step.

module FRP.YFrob.RobotSim.Simulator (
    SimbotController,
    simWorld,
    simWorld'	-- Temporary! Need to rethink interface.
) where

import Data.Array (Array, array, accumArray)

import FRP.Yampa
import FRP.Yampa.Geometry

import FRP.YFrob.Common.Diagnostics
import FRP.YFrob.Common.PhysicalDimensions
import FRP.YFrob.Common.RobotIO (RobotType, RobotId, BatteryStatus(..),
                                 rfOutOfRange)

import FRP.YFrob.RobotSim.IdentityList
import FRP.YFrob.RobotSim.Parser (SimInput)
import FRP.YFrob.RobotSim.IO
import FRP.YFrob.RobotSim.ObjectTemplate
import FRP.YFrob.RobotSim.Object
import FRP.YFrob.RobotSim.ObjectPhysics


type SimbotController = SimbotProperties -> SF SimbotInput SimbotOutput
-- !!! Maybe SimbotController should be redefined as follows:
-- type SimbotController = SimbotProperties -> SF SimbotInput (MR SimbotOutput)


------------------------------------------------------------------------------
-- Simulation of a given world
------------------------------------------------------------------------------

-- Have to reconsider this at a later point.
-- * Interaction with editor: maybe we rally want to keep all objects
--   alive across switches using our continuation machinery.
-- * Probably want to employ some form of embedding at some level,
--   (if nothing else so for allowing varying simulation speed), but
--   I still want to allow some interaction, obviously. The latter must
--   go outside the embed. And then, how could one possibly recover the
--   continations across an embed?
-- * At least we should use a continuation switch for implementing pause.
-- * It seems as if the signal functions representing different kinds of
--   objects are going to have different output types. Makes sense, I suppose,
--   but then we have to partition the world! Makes recovering continuations
--   even worse.
-- * One could move e.g. Event RobotTCO inside an object.
--   Not very nice, but would give all signal functions the same type.
-- * Otherwise, why have a single object type in the first place?
--   maybe it should be split?
-- * Regarding passing continuations to the edit mode: that assumes that the
--   signal function types are the same in both edit mode and in simulation
--   mode. That's arguable not modular. E.g. why would one want collision
--   and perception input in edit mode???
-- * For now, assume that the state of the world is passed on as a
--   WorldTemplate.


-- Representation of the static part or the world (less fixed walls).
type StaticWorld = [Object]


-- Simulation of a given world.
-- Arguments:
-- wt .........	Description of the world.
-- sca ........	Simbot controller for simbots of type A.
-- scb ........	Simbot controller for simbots of type B.
--
-- Signal inputs:
-- si .........	Simulator input.
--
-- Signal outputs:
-- #1.1 .......	The simulated animate and inanimate objects.
-- #1.2 .......	Text console output from the robots.
-- #2 .........	Termination event carrying the final state of the world.

simWorld :: WorldTemplate -> SimbotController -> SimbotController
            -> SF SimInput (([Object], Event [String]), Event WorldTemplate)
simWorld wt sca scb = proc si -> do
    -- !!! This structure allows for embedding, cont. switching for pause ...
    -- (objs, ems) <- embedSynch (simWorld' wt sca scb) dtis -< 4.0
    (objs, ems) <- simWorld' wt sca scb -< ()
    done        <- never -< ()	-- !!! Termination event. Fix!
    returnA -< ((objs, ems), done `tag` map objectToOT objs)
    -- where
    --	dtis = ((), repeat (0.01, Nothing))


simWorld' :: WorldTemplate -> SimbotController -> SimbotController
            -> SF a ([Object], Event [String])
simWorld' wt sca scb = proc _ -> do
    rec (rs, ertcos) <- simRobots wt sca scb sw -< (rs, bs)
        bs           <- simBalls wt sw          -< (rs, bs)
    let objs = ilElems rs ++ ilElems bs ++ sw
        ems  = fmap (concat . map formatRobotTCO) (catEvents ertcos)
    returnA -< (objs, ems)
    where
        sw = inanimateObjects wt

	formatRobotTCO :: (RobotType, RobotId, [String]) -> [String]
	formatRobotTCO (rtp, rid, ms) = map (pfx++) ms
	    where
		pfx = rtp ++ "." ++ show rid ++ ": "
	

inanimateObjects :: WorldTemplate -> StaticWorld
inanimateObjects wt = [ o | Just o <- map inanimObj wt]
    where
	inanimObj (OTBlock  {otPos = p}) = Just (block False p)
        inanimObj (OTNSWall {otPos = p}) = Just (nsWall False p)
        inanimObj (OTEWWall {otPos = p}) = Just (ewWall False p)
	inanimObj (OTVWall  {otPos = p}) = inanimObj (OTNSWall {otPos = p})
        inanimObj (OTHWall  {otPos = p}) = inanimObj (OTEWWall {otPos = p})
	inanimObj _                      = Nothing


------------------------------------------------------------------------------
-- Simulation of robots
------------------------------------------------------------------------------

{-
-- Shows how one can leverage IL to make a new collection type that
-- associates extra information with the objects, if necessary.
-- But didn't turn out to be a great idea in this case. Also, there is
-- an overhead for ALL uses of the data type, whereas the extra info might
-- only be needed for certain applications.

newtype ILR a = ILR (IL (RobotPhysicalProperties, a))
unILR (ILR il) = il


instance Functor ILR where
    fmap f = ILR . fmap (\(rpp, a) -> (rpp, f a)) . unILR


ilrElems :: ILR a -> [a]
ilrElems ilr = map (\(_, (_, a)) -> a) (ilAssocs (unILR ilr))


mapILR :: (ILKey -> RobotPhysicalProperties -> a -> b) -> ILR a -> ILR b
mapILR f ilr = ILR (mapIL (\(k,(rpp,a)) -> (rpp, f k rpp a)) (unILR ilr))
-}

data RobotPerception = RP {
    rpRanges      :: Array Int Distance,
    rpMaxRange    :: Distance,
    rpOtherRobots :: [(RobotType, RobotId, Angle, Distance)],
    rpBalls       :: [(Angle, Distance)]
}


-- A suitable default value when waiting for true sensor input (e.g. iPre).
-- Note that actual controller code should NOT assume that all other robots
-- are visible at all times, nor that rpMaxRange is not changing.
-- (Getting hold of rpMaxRange at the initial time step, e.g. through a
-- snapshot and then using it as a static value is a bad idea.)

rp_init :: RobotPerception
rp_init = RP {
              rpRanges      = array (0,-1) [],
              rpMaxRange    = 0,  -- rpRanges = [] => rfRange = rfOutOfRange
              rpOtherRobots = [],
              rpBalls       = []
          }


type RobotWorld = IL Object

type BallWorld = IL Object

type RobotTCO = (RobotType, RobotId, [String])


-- Simulation of robots.
-- Arguments:
-- wt .........	World description. Non-robots are ignored.
-- sca ........	Simbot controller for simbots of type A.
-- scb ........	Simbot controller for simbots of type B.
-- sw .........	Static part of the world.
--
-- Signal inputs:
-- rs .........	The simulated robots.
-- bs .........	The simulated balls.
--
-- Signal outputs:
-- #1 .........	The simulated robots (really the visible part of their state).
-- #2 .........	Text console output from the robots.

simRobots :: WorldTemplate
             -> SimbotController -> SimbotController
             -> StaticWorld
             -> SF (RobotWorld, BallWorld) (RobotWorld, [Event RobotTCO])
simRobots wt sca scb sw = proc rsbs -> do
    res <- par interactions (listToIL [sf | Just sf<-map simRobot wt]) -< rsbs
    returnA -< (fmap fst res, map snd (ilElems res))
    where
        simRobot (OTSimbotA {otRId = rid, otPos = p, otHdng = h}) =
	    Just (simSimbotA rid p h sca)
        simRobot (OTSimbotB {otRId = rid, otPos = p, otHdng = h}) =
	    Just (simSimbotB rid p h scb)
        simRobot _ = Nothing

        -- Computes the interaction with the rest of the world for each robot.
        interactions :: (RobotWorld, BallWorld) -> IL sf
			-> IL ((RobotPerception, Event ()), sf)
	interactions (rs, bs) sfs = mapIL interaction sfs
	    where
		interaction (k, sf) = ((rp, ce), sf)
		    where
			rp = robotPerception k r rs bs sw
			ce = robotCollision k r rs sw
			r  = case lookupIL k rs of
				 Just r  -> r
				 Nothing -> intErrSim
						"simRobots"
						"Can't find self in world."


-- Robot's percpetion of the world.
-- Arguments:
-- k ..........	The key of the robot for which to compute perception.
-- r .......... The robot for which to compute perception.
-- rs .........	The simulated robots.
-- bs .........	The simulated balls.
-- sw .........	Static part of the world.
--
-- Returns: RobotPerception representing the robot's view of the world.

robotPerception :: ILKey -> Object -> RobotWorld -> BallWorld -> StaticWorld
                   -> RobotPerception
robotPerception k r rs bs sw =
    RP {
        rpRanges      = sonarEchoes,
        rpMaxRange    = smr,
        rpOtherRobots = otherRobots (ilAssocs rs),
        rpBalls       = otherBalls (ilAssocs bs)
    }
    where
	p   = objPos r
	h   = objHdng r        
        rpp = objRPP r
	rr  = rppRadius rpp
        smr = rppRFMaxRange rpp
	n   = rppRFN rpp
	phi = 2 * pi / fromIntegral n

	-- Balls currently do not show up on the sonar. Too "small".
	sonarEchoes :: Array Int Distance
	sonarEchoes = accumArray min rfOutOfRange (0, n-1)
				 (sonarFixedWallEchoes
				  ++ sonarRobotEchoes
				  ++ sonarStaticWorldEchoes)

        sonarFixedWallEchoes :: [(Int, Distance)]
        sonarFixedWallEchoes =
	    -- !!! Compiler bug!?! [0.0, phi ..] yields a list of length 2.
	    -- !!! But it works in GHCi ...
	    [ (i, d)
            | i <- [0 .. n-1], let theta = (fromIntegral i) * phi,
              let d = sonarEchoFixedWall p rr (h + theta) phi, d <= smr ]

	sonarRobotEchoes :: [(Int, Distance)]
	sonarRobotEchoes =
            [ (rfIndex n theta, d)
	    | (k', r') <- ilAssocs rs, k' /= k,
              let (theta, d) = sonarEcho p h rr phi r', d <= smr ]
	
	sonarStaticWorldEchoes :: [(Int, Distance)]
	sonarStaticWorldEchoes =
            [ (rfIndex n theta, d)
	    | obj <- sw, let (theta, d) = sonarEcho p h rr phi obj,
	      d <= smr ]
	
        otherRobots ::
	    [(ILKey, Object)] -> [(RobotType, RobotId, Angle, Distance)]
	otherRobots []               = []
	otherRobots ((k', r') : krs) | k == k'   = otherRobots krs
				     | otherwise = (rtp', rid', phi, d)
					           : otherRobots krs
	    where
		rtp'    = objRType r'
		rid'    = objRId r'
		(d, h') = vector2RhoTheta (objPos r' .-. p)
		phi     = normalizeAngle (h' - h)
		
        otherBalls :: [(ILKey, Object)] -> [(Angle, Distance)]
	otherBalls [] = []
	otherBalls ((_, b) : kbs) = (phi, d) : otherBalls kbs
	    where
		(d, h') = vector2RhoTheta (objPos b .-. p)
		phi     = normalizeAngle (h' - h)
		

-- Robot's physical interaction with the world. Robots are currently not
-- affected by balls (they are considered "light").
-- Arguments:
-- k ..........	The key of the robot for which to compute interaction.
-- r .......... The robot for which to compute interaction.
-- rs .........	The simulated robots.
-- sw .........	Static part of the world.
--
-- Returns: Event indicating collision.

-- !!! Simultaneous hits currently ignored (mergeEvents).
-- !!! We currently do not make use of the impulse from the hit event
-- !!! since robots currently just stops.

robotCollision :: ILKey -> Object -> RobotWorld -> StaticWorld -> Event ()
robotCollision k r rs sw = mergeEvents collisions `tag` ()
    where
	collisions = hitFixedWall r
                     :  [ r `hit` obj | obj <- sw]
		     ++ [ r `hit` r' | (k', r') <- ilAssocs rs, k /= k' ]


------------------------------------------------------------------------------
-- Simulation of balls
------------------------------------------------------------------------------

-- Simulation of balls.
-- Arguments:
-- wt .........	World description. Non-balls are ignored.
-- sw .........	Static part of the world.
--
-- Signal inputs:
-- rs .........	The simulated robots.
-- bs .........	The simulated balls.
--
-- Signal outputs:
-- #1 .........	The simulated balls.

simBalls :: WorldTemplate -> StaticWorld
            -> SF (RobotWorld, BallWorld) BallWorld
simBalls wt sw =
    par interactions (listToIL [ simBall p | OTBall {otPos = p} <- wt ])
    where
        -- Computes the interaction with the rest of the world for each ball.
        interactions :: (RobotWorld, BallWorld) ->IL sf
                        ->IL (Event Velocity2,sf)
	interactions (rs, bs) sfs = mapIL interaction sfs
	    where
		interaction (k, sf) = (ballCollision k rs bs sw, sf)


-- Robot's physical interaction with the world. Robots are currently not
-- affected by balls (they are considered "light").
-- Arguments:
-- k ..........	The key of the ball for which to compute interaction.
-- rs .........	The simulated robots.
-- bs .........	The simulated balls.
-- sw .........	Static part of the world.
--
-- Returns: Event carrying collision impulse.

ballCollision :: ILKey -> RobotWorld -> BallWorld -> StaticWorld
                 -> Event Velocity2
ballCollision k rs bs sw = mergeEvents collisions
    where
	b = case lookupIL k bs of
		Just b  -> b
                Nothing -> intErrSim "ballCollision"
				     "Can't find ball in world."

	collisions = hitFixedWall b
                     :  [ b `hit` obj | obj <- sw]
		     ++ [ b `hit` b'  | (k', b') <- ilAssocs bs, k /= k' ]
		     ++ [ b `hit` r   | r <- ilElems rs ]


------------------------------------------------------------------------------
-- Simbot simulation
------------------------------------------------------------------------------

-- (Mainly) physical properties of a simbot.
data SimbotSpec = SS {
    ssRType    :: RobotType,    -- Robot type identification string.
    ssDiameter :: Length,       -- Distance between the wheels.
    ssAccMax   :: Acceleration, -- Maximal translational acceleration.
    ssWSMax    :: Speed         -- Maximal peripheral wheel speed.
}


-- Simulation of simbots of type A.
-- Arguments:
-- rid ........	Robot identity
-- p_0 ........	Initial position.
-- h_0 ........ Initial heading.
-- sc .........	Simbot controller.
--
-- Signal inputs:
-- rp .........	The simbot's perception of the world.
-- ce .........	Collision event.
--
-- Signal outputs:
-- #1 .........	Simbot object (really the visible part of the simbot state).
-- #2 ......... Text console output.

simSimbotA :: RobotId -> Position2 -> Heading -> SimbotController
              -> SF (RobotPerception, Event ()) (Object, Event RobotTCO)
simSimbotA rid p_0 h_0 sc = proc (rp, ce) -> do
    (p, h, v, ems) <- simSimbot rid p_0 h_0 ss sc -< (rp, ce)
    returnA -< (simbotA rtp rid False p h v, fmap (\ms -> (rtp,rid,ms)) ems)
    where
        rtp = simbotARType
        ss = SS {
		 ssRType    = rtp,
                 ssDiameter = simbotADiam,
		 ssAccMax   = simbotAAccMax,
		 ssWSMax    = simbotAWSMax
             }


-- Simulation of simbots of type B.
-- Arguments:
-- rid ........	Robot identity
-- p_0 ........	Initial position.
-- h_0 ........ Initial heading.
-- sc .........	Simbot controller.
--
-- Signal inputs:
-- rp .........	The simbot's perception of the world.
-- ce .........	Collision event.
--
-- Signal outputs:
-- #1 .........	Simbot object (really the visible part of the simbot state).
-- #2 ......... Text console output.

simSimbotB :: RobotId -> Position2 -> Heading -> SimbotController
              -> SF (RobotPerception, Event ()) (Object, Event RobotTCO)
simSimbotB rid p_0 h_0 sc = proc (rp, ce) -> do
    (p, h, v, ems) <- simSimbot rid p_0 h_0 ss sc -< (rp, ce)
    returnA -< (simbotB rtp rid False p h v, fmap (\ms -> (rtp,rid,ms)) ems)
    where
        rtp = simbotBRType
        ss = SS {
		 ssRType    = rtp,
                 ssDiameter = simbotBDiam,
		 ssAccMax   = simbotBAccMax,
		 ssWSMax    = simbotBWSMax
             }


-- Simbot simulation.
-- Arguments:
-- d ..........	Robot diameter.
-- a_max ......	Maximal translational acceleration.
-- ws_max .....	Maximal (peripheral) wheel speed.
-- rid ........	Robot identity
-- p_0 ........	Initial position.
-- h_0 ........ Initial heading.
-- ss ......... Simbot specification (physical properties).
-- sc .........	Simbot controller.
--
-- Signal inputs:
-- rp .........	The simbot's perception of the world.
-- ce .........	Collision event.
--
-- Signal outputs:
-- #1 .........	Current position.
-- #2 ......... Current heading.
-- #3 .........	Current translational velocity.
-- #4 ......... Text console output.

-- !!! Maybe "simbot" would be a more apt name in a sense since this really is
-- !!! a constructor for the true Simbots. The object on the output is just
-- !!! the "state".

-- !!! If simbot controllers return mergeable output, then change to
-- !!! so <- mrFinalize ^<< sc sp <- si () p h

simSimbot :: RobotId -> Position2 -> Heading -> SimbotSpec -> SimbotController
             -> SF (RobotPerception, Event ())
                   (Position2, Heading, Velocity2, Event [String])
simSimbot rid p_0 h_0 ss sc = proc (rp, ce) -> do
    -- Note the delay on the sensor inputs. We don't want to allow
    -- a controller to be control dependent on its output by just looking
    -- at sensor input. I.e. sensor input should always be well defined.
    st <- localTime -< ()
    rec s_pre   <- iPre False   -< s
        p_pre   <- iPre p_0     -< p
        h_pre   <- iPre h_0     -< h
        rp_pre  <- iPre rp_init -< rp
        so      <- sc sp        -< si st s_pre p_pre h_pre rp_pre
        wvs     <- wheelVelocities d                         -< soDM so
        (p,h,v) <- simbotDynamics d a_max ws_max p_0 h_0 v_0 -< (wvs, ce)
        s       <- isStuck                                   -< (ce, v)
    returnA -< (p, h, vector2Polar v h, soTCO so)
    where
        d      = ssDiameter ss
        a_max  = ssAccMax ss
        ws_max = ssWSMax ss
        v_0    = 0.0
	sp     = SP {
	       	     spRType    = ssRType ss,
	       	     spRId      = rid,
	       	     spDiameter = d,
	       	     spAccMax   = a_max,
	       	     spWSMax    = ws_max
	       	 }
        si st s p h rp = SI {
			  siSystemTime  = st,
		          siBattStat    = BSHigh,
                          siIsStuck     = s,
		          siPosition    = p,
                          siHeading     = h,
	                  siRanges      = rpRanges rp,
                          siMaxRange    = rpMaxRange rp,
			  siOtherRobots = rpOtherRobots rp,
                          siBalls       = rpBalls rp
                      }

        -- Somewhat complicated ... Note how isNoEvent is used to ensure
	-- that the edge detector is guaranteed to see a raising edge even
	-- if the velocity never got down to 0. (Using iEdge False would
	-- probably lead to a loop.)
        isStuck :: SF (Event (), Velocity) Bool
        isStuck =
            switch (constant False &&& arr fst) $ \_ ->
            switch (constant True
                    &&& (arr (\(ce, v) -> isNoEvent ce && abs v > 0.01)
                         >>> edge)) $ \_ ->
            isStuck


-- Potentially stateful conversion from high-level drive mode to low-level
-- control signals, i.e. desired wheel velocities. Makes it possible
-- to use signal functions for implementing high-level control algorithms for
-- high-level control modes, such as position control. See the old simulator
-- for some ideas on different kinds of control modes (although all state
-- less). For advaned modes, it is likely that more input signals are
-- needed, e.g. odometry.

wheelVelocities :: Length -> SF DriveMode (Velocity, Velocity)
wheelVelocities d = proc ddm -> do
    ec  <- edgeBy newDM DMBrake -< ddm
    wvs <- rSwitch wvBrake       -< (ddm, ec)
    returnA -< wvs
    where
	newDM DMBrake     DMBrake     = Nothing
        newDM _           DMBrake     = Just wvBrake
        newDM (DMDiff {}) (DMDiff {}) = Nothing
        newDM _           (DMDiff {}) = Just wvDiff
        newDM (DMTR {})   (DMTR {})   = Nothing
        newDM _           (DMTR {})   = Just (wvTR d)


	wvBrake :: SF DriveMode (Velocity, Velocity)
	wvBrake = constant (0.0, 0.0)
	
	-- Differential control, essentially the identity signal function.
	wvDiff :: SF DriveMode (Velocity, Velocity)
	wvDiff = proc (DMDiff {dmdLWV = v_ld, dmdRWV = v_rd}) -> do
	    returnA -< (v_ld, v_rd)
	
	-- Translational and rotational velocity control.
	-- d ..........	Robot diameter.
	wvTR :: Length -> SF DriveMode (Velocity, Velocity)
	wvTR d = proc (DMTR {dmtrTV = tv_d, dmtrRV = rv_d}) -> do
	    returnA -< (tv_d - r * rv_d, tv_d + r * rv_d)
	    where
		r = d / 2
	

------------------------------------------------------------------------------
-- Ball simulation
------------------------------------------------------------------------------

-- Simulation of simbots of type A.
-- Arguments:
-- p_0 ........	Initial position.
--
-- Signal inputs:
-- ei .........	Impulse event. Instantaneous change in momentum. Since
--		the mass is constant but implicit, the impulse is represented
--		by an instantaneous change in velocity.
--
-- Signal outputs:
-- #1 .........	Ball (really the visible part of the ball state).

simBall :: Position2 -> SF (Event Velocity2) Object
simBall p_0 = proc ei -> do
    -- Why delay needed? impulseIntegral too strict? Or should objVel field
    -- be non-strict? But the latter seems to lead to a loop ...
    -- !!! 2003-01-25: OK, one could argue that impulseIntegral is too
    -- !!! strict, but not being strict here would lead to unwanted
    -- !!! delays. Another way of looking at the problem is that we
    -- !!! probably are trying to compute the size of the impulse
    -- !!! which directly will affect the velocity in terms of the
    -- !!! very same velocity at the very same point in time.
    -- !!! So whoever computes the strength of the impulse, should perhaps
    -- !!! use the previous velocity. A simple bouncing ball model shows this.
    eid <- iPre noEvent -< ei
    (p, v) <- ballDynamics 0.4 0.1 p_0 zeroVector -< eid
    returnA -< ball False p v


------------------------------------------------------------------------------
-- Old stuff
------------------------------------------------------------------------------

{-
type WCont i a = SigBRef i World -> Cont i a

sWorld :: SimInput i => SimbotController -> SimbotController -> Cont i World
sWorld rcA rcB w = loopB $ \self -> zListToListZ (map (sObj rsA rsB self) w)
    where
        rsA = RobotSpec {
	          rsDiameter   = robotADiam,
		  rsAccMax     = robotAAccMax,
		  rsWSMax      = robotAWSMax,
		  rsAngAccMax  = robotAAngAccMax,
		  rsRDAngles   = robotARDAngles,
		  rsModel      = robotModel,
		  rsController = rcA
	      }
        rsB = RobotSpec {
	          rsDiameter   = robotBDiam,
		  rsAccMax     = robotBAccMax,
		  rsWSMax      = robotBWSMax,
		  rsAngAccMax  = robotBAngAccMax,
		  rsRDAngles   = robotBRDAngles,
		  rsModel      = robotModel,
		  rsController = rcB
	      }


sWorld2 :: SimInput i =>
    SimbotModel -> SimbotController -> SimbotModel -> SimbotController ->
    Cont i World
sWorld2 rmA rcA rmB rcB w =
    loopB $ \self -> zListToListZ (map (sObj rsA rsB self) w)
    where
        rsA = RobotSpec {
	          rsDiameter   = robotADiam,
		  rsAccMax     = robotAAccMax,
		  rsWSMax      = robotAWSMax,
		  rsAngAccMax  = robotAAngAccMax,
		  rsRDAngles   = robotARDAngles,
		  rsModel      = rmA,
		  rsController = rcA
	      }
        rsB = RobotSpec {
	          rsDiameter   = robotBDiam,
		  rsAccMax     = robotBAccMax,
		  rsWSMax      = robotBWSMax,
		  rsAngAccMax  = robotBAngAccMax,
		  rsRDAngles   = robotBRDAngles,
		  rsModel      = rmB,
		  rsController = rcB
	      }


-- Object simulation
sObj :: SimInput i => RobotSpec -> RobotSpec -> WCont i Object
sObj rsA rsB wr obj
    | obj `oInClass` ClsObst   = lift0 obj        -- Obstacles don't move.
    | obj `oInClass` ClsRobotA = sRobot rsA wr obj
    | obj `oInClass` ClsRobotB = sRobot rsB wr obj
    | obj `oInClass` ClsBall   = sBall wr obj
    | otherwise = simulatorErr "sObj" "Unknown object class."
-}


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

intErrSim :: String -> String -> a
intErrSim = intErr "RobotSim.Simulator"
