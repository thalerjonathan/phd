{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE ScopedTypeVariables               #-}
module FRP.FrABS.Rendering.GlossSimulator (
	simulateAndRender,
	simulateStepsAndRender,
	
    debugAndRender,

	StepCallback,
	RenderFrame
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Simulation.Simulation

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import FRP.Yampa

import Data.IORef
import Control.Concurrent
import Control.Monad

type RenderFrame s e = ((Int, Int) -> [AgentObservable s] -> e -> GLO.Picture)
type StepCallback s e = (([AgentObservable s], e) -> ([AgentObservable s], e) -> IO ())

type RenderFrameInternal s e = ([AgentObservable s] -> e -> GLO.Picture)

simulateAndRender :: [AgentDef s m e] 
						-> e 
						-> SimulationParams e
						-> Double
						-> Int
						-> String
						-> (Int, Int)
						-> RenderFrame s e
						-> Maybe (StepCallback s e)
						-> IO ()
simulateAndRender initAdefs 
					  e 
					  params 
					  dt 
					  freq 
					  winTitle 
					  winSize
					  renderFunc
					  mayClbk =
	do
		outRef <- newIORef (initEmptyAgentObs, e) -- :: IO (IORef ([AgentObservable s], e))
		hdl <- processIOInit initAdefs e params (nextIteration mayClbk outRef)

		if freq > 0 then
			simulateIO (displayGlossWindow winTitle winSize)
				GLO.black
				freq
				([], e)
				(modelToPicture (renderFunc winSize))
				(nextFrameSimulateWithTime dt hdl outRef)
			else
				animateIO (displayGlossWindow winTitle winSize)
					GLO.black
					(nextFrameSimulateNoTime (renderFunc winSize) dt hdl outRef)
					(\_ -> return () )
		return ()

		where
			-- NOTE: need this function otherwise would get a compilation error when creating newIORef (see above)
			initEmptyAgentObs :: [AgentObservable s]
			initEmptyAgentObs = []

simulateStepsAndRender :: [AgentDef s m e] 
							-> e  
							-> SimulationParams e
							-> Double
							-> Int
							-> String
							-> (Int, Int)
							-> RenderFrame s e
							-> IO ()
simulateStepsAndRender initAdefs 
				       e 
				       params 
				       dt 
				       steps 
				       winTitle 
				       winSize
				       renderFunc =
	do
		let ass = processSteps initAdefs e params dt steps
		let (finalAobs, finalEnv) = last ass
		let pic = renderFunc winSize finalAobs finalEnv 

		GLO.display (displayGlossWindow winTitle winSize)
				GLO.black
				pic

debugAndRender :: forall s e m .
                (Show s, Read s, Show e, Read e)
                => [AgentDef s m e] 
                -> e 
                -> SimulationParams e
                -> Double
                -> Int
                -> String
                -> (Int, Int)
                -> RenderFrame s e
                -> IO ()
debugAndRender initAdefs 
                  e 
                  params 
                  dt 
                  freq 
                  winTitle 
                  winSize
                  renderFunc =
    do
        let initAobs = map (\ad -> (adId ad, adState ad)) initAdefs
        initPic <- modelToPicture (renderFunc winSize) (initAobs, e)

        renderOutputRef <- newIORef initPic
        nextStepVar <- newEmptyMVar

        _ <- forkIO $ processDebugInternal 
                        initAdefs e params (\_ -> takeMVar nextStepVar) (outputRender renderOutputRef)

        if freq > 0 then
            simulateIO 
                (displayGlossWindow winTitle winSize)
                GLO.black
                freq
                0
                (const $ readIORef renderOutputRef)
                (\_ timeDelta t -> putMVar nextStepVar (dt, Nothing) >> return (t + timeDelta))
            else
                animateIO 
                    (displayGlossWindow winTitle winSize)
                    GLO.black
                    (\_ -> putMVar nextStepVar (dt, Nothing) >> readIORef renderOutputRef)
                    (\_ -> return ())
        -- return ()

        where
            -- NOTE: need this function otherwise would get a compilation error when creating newIORef (see above)
            initEmptyAgentObs :: [AgentObservable s]
            initEmptyAgentObs = []

            outputRender :: IORef Picture -> Bool -> ([AgentObservable s], e) -> IO Bool
            outputRender renderOutputRef updated out = 
                when updated 
                    (do
                        pic <- modelToPicture (renderFunc winSize) out
                        writeIORef renderOutputRef pic) >> return False
                        

nextIteration :: Maybe (StepCallback s e)
					-> IORef ([AgentObservable s], e)
                    -> ReactHandle () ([AgentObservable s], e)
					-> Bool
					-> ([AgentObservable s], e)
					-> IO Bool
nextIteration (Just clbk) obsRef _ _ curr@(currAobs, currEnv) = 
    do
    	(prevAobs, prevEnv) <- readIORef obsRef
    	clbk (prevAobs, prevEnv) (currAobs, currEnv)
        writeIORef obsRef curr
        return False
nextIteration Nothing obsRef _ _ curr = writeIORef obsRef curr >> return False

nextFrameSimulateWithTime :: Double 
								-> ReactHandle () ([AgentObservable s], e)
	                            -> IORef ([AgentObservable s], e)
	                            -> ViewPort
	                            -> Float
	                            -> ([AgentObservable s], e)
	                            -> IO ([AgentObservable s], e)
nextFrameSimulateWithTime dt hdl obsRef _ _ _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aobs <- readIORef obsRef
        return aobs

nextFrameSimulateNoTime :: RenderFrameInternal s e
                            -> Double
                            -> ReactHandle () ([AgentObservable s], e)
                            -> IORef ([AgentObservable s], e)
                            -> Float
                            -> IO Picture
nextFrameSimulateNoTime renderFunc dt hdl obsRef _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aobs <- readIORef obsRef
        modelToPicture renderFunc aobs

modelToPicture :: RenderFrameInternal s e
					-> ([AgentObservable s], e) 
					-> IO GLO.Picture
modelToPicture renderFunc (aobs, e) = return $ renderFunc aobs e

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow title winSize = (GLO.InWindow title winSize (0, 0))