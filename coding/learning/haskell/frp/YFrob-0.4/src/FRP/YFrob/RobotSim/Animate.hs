{-
******************************************************************************
*                        Y F R O B / R O B O T S I M                         *
*                                                                            *
*       Module:         Animate						     *
*       Purpose:        Animation of graphical signal functions.	     *
*       Author:		Henrik Nilsson					     *
*                                                                            *
******************************************************************************
-}

-- For now, simple animation, no support for I/O. Maybe the way to go is
-- an IOTask. See notes in Yampa.
--
-- We could probably have got by without breaking the Event abstraction.
-- If nothing else, turning a maybe into event is easily done by a
-- signal function which could be added as a pre-processor. E.g.
-- eventBy (const id) >>> sf

-- Approach: The signal function is sampled as frequently as possible. It's
-- the OS's task to allocate resources, so we can just as well use up all the
-- CPU cycles we get. But since drawing is very time consuming, we draw at a
-- fixed, user-defineable, presumably lower rate, thus allowing the user to
-- control the ratio between the cycles spent on drawing and the cycles spent
-- on editing/simulation. This approach may result in rather uneven sampling
-- intervals, but embedSynch can be used to provide a stable time base when
-- that is important, e.g. for simulation, as long as there are enough cycles
-- on average to keep up.
--
-- For some reason, context switching does not work as it should unless
-- the window tick mechanism is enabled. For that reason, we use a high
-- frequency tick (1kHz). (Alternatively, passing the -C runtime flag (e.g.
-- +RTS -C1) forces regular context switches. Moreover, getting events
-- without delay seems to require yielding to ensure that the thread
-- receiving them gets a chance to run. This can be done using yield prior to
-- galling HGL.maybeGetWindowEvent. Alternatively, we can get the window tick,
-- and since the tick frequency is high, no major waiting should ensue. This
-- is the current method, although it seems as if this method means that
-- window close events often will be missed.

module FRP.YFrob.RobotSim.Animate (WinInput, animate) where

import Control.Monad (when)
-- import Data.Maybe (isJust, fromJust)
-- import Posix (SysVar(..), ProcessTimes, ClockTick,
--               getSysVar, getProcessTimes, elapsedTime)
-- import Concurrent (yield)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Graphics.HGL as HGL

import FRP.YFrob.Common
-- Actually, Events are not abstract in the publicly available Yampa 0.9.2.3
import FRP.Yampa.Internals	-- Breaking the Event abstraction barrier here!
import FRP.Yampa.Forceable


type WinInput = Event HGL.Event


------------------------------------------------------------------------------
-- Animation
------------------------------------------------------------------------------

-- Animate a signal function
-- fr .........	Frame rate.
-- title ...... Window title.
-- width ...... Window width in pixels.
-- height ..... Window height in pixels.
-- render ..... Renderer; invoked at the frame rate.
-- tco ........ Text Console Output; invoked at every step.
-- sf ......... Signal function to animate.

-- !!! Note: it would be easy to add an argument (a -> IO b) as well, allowing
-- !!! arbitrary I/O. One could even replace the text output by such a
-- !!! function. Could one possibly somehow get data back into the signal
-- !!! function by means of continuations? Or maybe the IOTask monad is the
-- !!! way to go, with a special "reactimateIOTask".

animate :: Forceable a =>
    Frequency -> String -> Int -> Int
    -> (a -> HGL.Graphic)
    -> (a -> [String])
    -> (SF WinInput a)
    -> IO ()
animate fr title width height render tco sf = HGL.runGraphics $
    do
        win <- HGL.openWindowEx title
			      Nothing			-- Initial position.
			      (width, height)		-- Window size.
			      HGL.DoubleBuffered	-- Painfully SLOW!!!
			      -- HGL.Unbuffered		-- Flickers!
			      (Just 1)	        	-- For scheduling!?!
        (init, getTimeInput, isClosed) <- mkInitAndGetTimeInput win
	reactimate init
                   getTimeInput
                   (\_ ea@(e,a) -> do
                       updateWin render win ea
		       forAll (tco a) putStrLn
                       isClosed)
		   (repeatedly (1/fr) () &&& sf)
        HGL.closeWindow win


------------------------------------------------------------------------------
-- Support for reading time and input
------------------------------------------------------------------------------

mkInitAndGetTimeInput
    :: HGL.Window
       -> IO (IO WinInput, Bool -> IO (DTime,Maybe WinInput), IO Bool)
mkInitAndGetTimeInput win = do
    -- clkRes   <- fmap fromIntegral (getSysVar ClockTick)
    let clkRes = 1000
    tpRef     <- newIORef errInitNotCalled
    wepRef    <- newIORef errInitNotCalled
    weBufRef  <- newIORef Nothing
    closedRef <- newIORef False
    let init = do
            t0 <- getElapsedTime
	    writeIORef tpRef t0
            mwe <- getWinInput win weBufRef
            writeIORef wepRef mwe
            return (maybeToEvent mwe)
    let getTimeInput _ = do
	    tp <- readIORef tpRef
	    t  <- getElapsedTime `repeatUntil` (/= tp) -- Wrap around possible!
	    let dt = if t > tp then fromIntegral (t-tp)/clkRes else 1/clkRes
	    writeIORef tpRef t
	    mwe  <- getWinInput win weBufRef
	    mwep <- readIORef wepRef
            writeIORef wepRef mwe
	    -- putStrLn ("dt = " ++ show dt)
            -- when (isJust mwe) (putStrLn ("Event = " ++ show (fromJust mwe)))
	    -- Simplistic "delta encoding": detects only repeated NoEvent.
            case (mwep, mwe) of
                (Nothing, Nothing) -> return (dt, Nothing)
		(_, Just HGL.Closed) -> do
					    writeIORef closedRef True
					    return (dt, Just(maybeToEvent mwe))
	        _                    -> return (dt, Just (maybeToEvent mwe))
    return (init, getTimeInput, readIORef closedRef)
    where
	errInitNotCalled = intErr "RSAnimate"
				  "mkInitAndGetTimeInput"
                                  "Init procedure not called."

        -- Accurate enough? Resolution seems to be 0.01 s, which could lead
	-- to substantial busy waiting above.
        -- getElapsedTime :: IO ClockTick
        -- getElapsedTime = fmap elapsedTime getProcessTimes

        -- Use this for now. Have seen delta times down to 0.001 s. But as
	-- the complexity of the simulator signal function gets larger, the
	-- processing time for one iteration will presumably be > 0.01 s,
	-- and a clock resoltion of 0.01 s vs. 0.001 s becomes a non issue.
	getElapsedTime :: IO HGL.Time
	getElapsedTime = HGL.getTime

        maybeToEvent :: Maybe a -> Event a
	maybeToEvent = maybe NoEvent Event


-- Get window input, with "redundant" mouse moves removed.
getWinInput :: HGL.Window -> IORef (Maybe HGL.Event) -> IO (Maybe HGL.Event)
getWinInput win weBufRef = do
    mwe <- readIORef weBufRef
    case mwe of
	Just _  -> do
	    writeIORef weBufRef Nothing
	    return mwe
	Nothing -> do
	    mwe' <- gwi win
	    case mwe' of
	        Just (HGL.MouseMove {}) -> mmFilter mwe'
		_                       -> return mwe'
    where
	mmFilter jmme = do
	    mwe' <- gwi win
	    case mwe' of
		Nothing                 -> return jmme
		Just (HGL.MouseMove {}) -> mmFilter mwe'
		Just _                  -> writeIORef weBufRef mwe'
                                           >> return jmme

	-- Seems as if we either have to yield or wait for a tick in order
	-- to ensure that the thread receiving events gets a chance to
	-- work. For some reason, yielding seems to result in window close
	-- events getting through, wheras waiting often means they don't.
        -- Maybe the process typically dies before the waiting time is up in
        -- the latter case?
        gwi win = do
	    -- yield
            HGL.getWindowTick win
	    mwe <- HGL.maybeGetWindowEvent win
	    return mwe


------------------------------------------------------------------------------
-- Support for output
------------------------------------------------------------------------------

-- Need to force non-displayed elements to avoid space leaks.
-- We also explicitly force displayed elements in case the renderer does not
-- force everything.
updateWin ::
    Forceable a => (a -> HGL.Graphic) -> HGL.Window -> (Event (), a) -> IO ()
updateWin render win (e, a) = when (force a `seq` isEvent e)
                                   (HGL.setGraphic win (render a))
