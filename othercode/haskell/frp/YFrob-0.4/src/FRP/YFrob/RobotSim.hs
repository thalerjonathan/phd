{-# LANGUAGE Arrows #-}

{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:		RobotSim					     *
*       Purpose:	Top-level robot simulator module.		     *
*			To be imported into a top-level module supplying     *
*			robot control code.				     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

module FRP.YFrob.RobotSim (
    module FRP.YFrob.Common,
    SimbotProperties,
    SimbotInput,
    SimbotOutput,
    SimbotController,
    WorldTemplate,
    ObjectTemplate(..),
    BoundingBox,
    worldXMin,
    worldYMin,
    worldXMax,
    worldYMax,
    boundingBox,
    runSim,
    playSoccer		-- Probably temporary.
) where

import qualified Graphics.HGL as HGL

import FRP.YFrob.Common

import FRP.YFrob.RobotSim.WorldGeometry
import FRP.YFrob.RobotSim.Colors
import FRP.YFrob.RobotSim.ColorBindings
import FRP.YFrob.RobotSim.ObjectTemplate    -- !!! Only to supprt playSoccer!
import FRP.YFrob.RobotSim.Object (Object(..))
import FRP.YFrob.RobotSim.IO
import FRP.YFrob.RobotSim.RenderFixedWalls (fixedWalls)
import FRP.YFrob.RobotSim.RenderObject (renderObjects)
import FRP.YFrob.RobotSim.Animate
import FRP.YFrob.RobotSim.Parser
import FRP.YFrob.RobotSim.Simulator (SimbotController, simWorld, simWorld')


title :: String
title = "Y F R O B / R O B O T S I M"
width, height :: Int
width = 600
height = 600

fr :: Frequency
fr = 20 -- Hz


-- !!! Providing no initial world should be used to invoke the editor.
-- !!! Eventually, we might want a different runSim to do that. Or pass a flag.
-- !!! We might also want to make runSim take a single controller,
-- !!! whereas runSim2 should take 2.

runSim :: Maybe WorldTemplate -> SimbotController -> SimbotController -> IO ()
runSim mwt sca scb =
    animate fr title width height
	-- !!! HGL seems to leak memory. Invoke only fixedWalls below
	-- !!! to see a MUCH slower growth rate ... Sigh.
        (\(cfbs, (objs, _)) -> renderObjects objs
		               `HGL.overGraphic` HGL.text (0,0) cfbs
                               `HGL.overGraphic` fixedWalls)
        (\(_, (_, ems)) -> event [] id ems)
        (parseWinInput >>> cmdString &&& simulator wt)
    where
{-
        -- For testing purposes. Draws initial world repeatedly.
        simulator wt =
	    (proc inp -> do
                 (w, _) <- simWorld wt sca scb -< inp
		 e <- now () -< ()
                 returnA -< (w, e `tag` w)
            ) `switch` (\w -> constant w)
-}
        simulator wt = switch (simWorld wt sca scb) $ editor

        editor wt = switch (constant undefined &&& now wt) $ simulator

	wt = maybe dfltWT id mwt

        dfltWT = [
	    OTSimbotA {
		otRId  = 1,
		otPos  = Point2 (-2.5) 0.0,
		otHdng = 0.0
	    },
	    OTSimbotB {
		otRId  = 1,
		otPos  = (Point2 2.5    0.0),
		otHdng = (-pi)
	    }
          ]


------------------------------------------------------------------------------
-- Soccer game setup
------------------------------------------------------------------------------

soccerWorld :: WorldTemplate
soccerWorld = [
    OTSimbotB {otRId = 1,  otPos = Point2 (-4) 0,    otHdng=0},
    OTSimbotB {otRId = 2,  otPos = Point2 (-2) 3,    otHdng=0},
    OTSimbotB {otRId = 3,  otPos = Point2 (-2) (-3), otHdng=0},
    OTSimbotB {otRId = 11, otPos = Point2 4    0,    otHdng=pi},
    OTSimbotB {otRId = 12, otPos = Point2 2    (-3), otHdng=pi},
    OTSimbotB {otRId = 13, otPos = Point2 2    3,    otHdng=pi},
    OTBlock   {otPos = Point2 (-4.75) 1.25},
    OTBlock   {otPos = Point2 (-4.25) 1.25},
    OTBlock   {otPos = Point2 (-4.75) (-1.25)},
    OTBlock   {otPos = Point2 (-4.25) (-1.25)},
    OTBlock   {otPos = Point2 4.75    1.25},
    OTBlock   {otPos = Point2 4.25    1.25},
    OTBlock   {otPos = Point2 4.75    (-1.25)},
    OTBlock   {otPos = Point2 4.25    (-1.25)},
    OTBall    {otPos = Point2 0 0}
  ]


playSoccer :: [SimbotController] -> [SimbotController] -> IO ()
playSoccer team1 team2 =
    animate fr title width height
        (\((score1, score2, mg), (objs, _)) ->
	    renderObjects objs
            `HGL.overGraphic` (maybe HGL.emptyGraphic
                                     (\g->HGL.text (230,250) g) mg)
	    `HGL.overGraphic` HGL.text (0,0) (show score1)
	    `HGL.overGraphic` HGL.text (580,0) (show score2)
            `HGL.overGraphic` fixedWalls)
        (\(_, (_, ems)) -> event [] id ems)
        (simloop soccerWorld 0 0)
    where
        simloop :: WorldTemplate -> Int -> Int
	    	   -> SF a ((Int, Int, Maybe String),
                            ([Object], Event [String]))
        simloop wt score1 score2 =
            switch (simulator wt score1 score2) $ \((score1, score2), w) ->
            switch (constant ((score1, score2, Just "G O O O O O A L ! ! !"),
			      (w, noEvent))
                    &&& after 5 ((score1, score2),
                                 map objectToOT w)) $ \((score1,score2),wt) ->
            simloop (ballToCenter wt) score1 score2      

        simulator wt score1 score2 = proc _ -> do
	    (objs, ems) <- simWorld' wt rcStop rc -< ()
            scored1     <- edgeTag (score1, score2 + 1) -< ballInLeftGoal objs
            scored2     <- edgeTag (score1 + 1, score2) -< ballInRightGoal objs
	    returnA -< (((score1, score2, Nothing), (objs, ems)),
			(scored1 `merge` scored2) `attach` objs)

	rc rp = let rid = rpId rp
                in
		    if 1 <= rid && rid <= length team1 then
			(team1 !! (rid - 1)) rp
		    else if 11 <= rid && rid <= 10 + length team2 then
			(team2 !! (rid - 11)) rp
		    else
			rcStop rp

	rcStop :: SimbotController
	rcStop _ = constant (mrFinalize ddBrake)

	ballToCenter []                = []
        ballToCenter (OTBall {} : ots) = OTBall { otPos = Point2 0 0 } : ots
        ballToCenter (ot        : ots) = ot : ballToCenter ots

        ballInLeftGoal [] = False
        ballInLeftGoal (ObjBall { objPos = Point2 x y } : _) =
	    x <= (-4.2) && (-1) <= y && y <= 1
        ballInLeftGoal (_ : objs) = ballInLeftGoal objs

        ballInRightGoal [] = False
        ballInRightGoal (ObjBall { objPos = Point2 x y } : _) =
	    x >= 4.2 && (-1) <= y && y <= 1
        ballInRightGoal (_ : objs) = ballInRightGoal objs
