module FrABS.Rendering.GlossSimulator (
	simulateAndRender,
	simulateStepsAndRender,
	
	RenderFrame
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Interface.IO.Display

import FRP.Yampa

import Data.IORef

type RenderFrame s m ec l = ((Int, Int) -> [AgentOut s m ec l] -> Environment ec l -> GLO.Picture)
type RenderFrameInternal s m ec l = ([AgentOut s m ec l] -> Environment ec l -> GLO.Picture)

simulateAndRender :: [AgentDef s m ec l] 
						-> Environment ec l 
						-> SimulationParams ec l
						-> Double
						-> Int
						-> String
						-> (Int, Int)
						-> RenderFrame s m ec l
						-> IO ()
simulateAndRender initAdefs 
				  initEnv 
				  params 
				  dt 
				  freq 
				  winTitle 
				  winSize
				  renderFunc =
	do
		outRef <- newIORef (initEmptyAgentOuts, initEnv) -- :: IO (IORef ([AgentOut s m ec l], Environment ec l))
		hdl <- processIOInit initAdefs initEnv params (nextIteration outRef)

		if freq > 0 then
			simulateIO (displayGlossWindow winTitle winSize)
				GLO.white
				freq
				([], initEnv)
				(modelToPicture (renderFunc winSize))
				(nextFrameSimulateWithTime dt hdl outRef)
			else
				animateIO (displayGlossWindow winTitle winSize)
					GLO.white
					(nextFrameSimulateNoTime (renderFunc winSize) dt hdl outRef)
					(\_ -> return () )
		return ()

		where
			-- NOTE: need this function otherwise would get a compilation error when creating newIORef (see above)
			initEmptyAgentOuts :: [AgentOut s m ec l]
			initEmptyAgentOuts = []

simulateStepsAndRender :: [AgentDef s m ec l] 
							-> Environment ec l  
							-> SimulationParams ec l
							-> Double
							-> Int
							-> String
							-> (Int, Int)
							-> RenderFrame s m ec l
							-> IO ()
simulateStepsAndRender initAdefs 
				       initEnv 
				       params 
				       dt 
				       steps 
				       winTitle 
				       winSize
				       renderFunc =
	do
		let ass = processSteps initAdefs initEnv params dt steps
		let (finalAous, finalEnv) = last ass
		let pic = renderFunc winSize finalAous finalEnv 

		GLO.display (displayGlossWindow winTitle winSize)
				GLO.white
				pic

nextIteration :: IORef ([AgentOut s m ec l], Environment ec l )
                    -> ReactHandle ([AgentIn s m ec l], Environment ec l) ([AgentOut s m ec l], Environment ec l)
					-> Bool
					-> ([AgentOut s m ec l], Environment ec l )
					-> IO Bool
nextIteration outRef _ _ aep@(aouts, _) = 
    do
        writeIORef outRef aep
        return False

nextFrameSimulateWithTime :: Double 
								-> ReactHandle ([AgentIn s m ec l], Environment ec l) ([AgentOut s m ec l], Environment ec l)
	                            -> IORef ([AgentOut s m ec l], Environment ec l)
	                            -> ViewPort
	                            -> Float
	                            -> ([AgentOut s m ec l], Environment ec l)
	                            -> IO ([AgentOut s m ec l], Environment ec l)
nextFrameSimulateWithTime dt hdl outRef _ _ _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        return aouts

nextFrameSimulateNoTime :: RenderFrameInternal s m ec l
							-> Double
							-> ReactHandle ([AgentIn s m ec l], Environment ec l) ([AgentOut s m ec l], Environment ec l)
			                -> IORef ([AgentOut s m ec l], Environment ec l)
			                -> Float
			                -> IO Picture
nextFrameSimulateNoTime renderFunc dt hdl outRef _ = 
    do
        react hdl (dt, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        modelToPicture renderFunc aouts

modelToPicture :: RenderFrameInternal s m ec l
					-> ([AgentOut s m ec l], Environment ec l) 
					-> IO GLO.Picture
modelToPicture renderFunc (aouts, env) = 
    do
        return $ renderFunc aouts env

displayGlossWindow :: String -> (Int, Int) -> GLO.Display
displayGlossWindow title winSize = (GLO.InWindow title winSize (0, 0))