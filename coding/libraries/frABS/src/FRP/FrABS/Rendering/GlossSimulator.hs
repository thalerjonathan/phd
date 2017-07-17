module FRP.FrABS.Rendering.GlossSimulator (
	simulateAndRender,
	simulateStepsAndRender,
	
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

type RenderFrame s m e = ((Int, Int) -> [AgentOut s m e] -> e -> GLO.Picture)
type StepCallback s m e = (([AgentOut s m e], e) -> ([AgentOut s m e], e) -> IO ())

type RenderFrameInternal s m e = ([AgentOut s m e] -> e -> GLO.Picture)

simulateAndRender :: [AgentDef s m e] 
						-> e 
						-> SimulationParams e
						-> Double
						-> Int
						-> String
						-> (Int, Int)
						-> RenderFrame s m e
						-> Maybe (StepCallback s m e)
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
		outRef <- newIORef (initEmptyAgentOuts, e) -- :: IO (IORef ([AgentOut s m e], Environment e))
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
			initEmptyAgentOuts :: [AgentOut s m e]
			initEmptyAgentOuts = []

simulateStepsAndRender :: [AgentDef s m e] 
							-> e  
							-> SimulationParams e
							-> Double
							-> Int
							-> String
							-> (Int, Int)
							-> RenderFrame s m e
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
		let (finalAous, finalEnv) = last ass
		let pic = renderFunc winSize finalAous finalEnv 

		GLO.display (displayGlossWindow winTitle winSize)
				GLO.black
				pic

nextIteration :: Maybe (StepCallback s m e)
					-> IORef ([AgentOut s m e], e)
                    -> ReactHandle ([AgentIn s m e], e) ([AgentOut s m e], e)
					-> Bool
					-> ([AgentOut s m e], e)
					-> IO Bool
nextIteration (Just clbk) outRef _ _ curr = 
    do
    	prev <- readIORef outRef
    	clbk prev curr
        writeIORef outRef curr
        return False
nextIteration Nothing outRef _ _ curr = writeIORef outRef curr >> return False

nextFrameSimulateWithTime :: Double 
								-> ReactHandle ([AgentIn s m e], e) ([AgentOut s m e], e)
	                            -> IORef ([AgentOut s m e], e)
	                            -> ViewPort
	                            -> Float
	                            -> ([AgentOut s m e], e)
	                            -> IO ([AgentOut s m e], e)
nextFrameSimulateWithTime dt hdl outRef _ _ _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        return aouts

nextFrameSimulateNoTime :: RenderFrameInternal s m e
							-> Double
							-> ReactHandle ([AgentIn s m e], e) ([AgentOut s m e], e)
			                -> IORef ([AgentOut s m e], e)
			                -> Float
			                -> IO Picture
nextFrameSimulateNoTime renderFunc dt hdl outRef _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        modelToPicture renderFunc aouts

modelToPicture :: RenderFrameInternal s m e
					-> ([AgentOut s m e], e) 
					-> IO GLO.Picture
modelToPicture renderFunc (aouts, e) = 
    do
        return $ renderFunc aouts e

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow title winSize = (GLO.InWindow title winSize (0, 0))