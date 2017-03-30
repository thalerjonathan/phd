module SugarScape.SugarScapeRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Env.Environment

import SugarScape.SugarScapeModel
import SugarScape.SugarScapeInit
import SugarScape.SugarScapeRenderer as Renderer

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import Data.IORef
import System.IO
import System.Random
import Debug.Trace

winSize = (800, 800)
winTitle = "SugarScape Chapter II FrABS"
renderCircles = True

rngSeed = 42
agentCount = 250
envSize = (50, 50)

parallelStrategyFlag = False -- NOTE: sugarscape will not give correct result when run with parallel update-strategy

runSugarScapeWithRendering :: IO ()
runSugarScapeWithRendering = do
                                hSetBuffering stdout NoBuffering
                                hSetBuffering stderr NoBuffering
                                initRng rngSeed
                                (as, env) <- createSugarScape agentCount envSize

                                outRef <- (newIORef ([])) :: (IO (IORef ([SugarScapeAgentOut])))
                                hdl <- processIOInit as env parallelStrategyFlag (nextIteration outRef)

                                --simulateAndRenderNoTime hdl outRef
                                simulateAndRenderWithTime hdl outRef

nextIteration :: IORef ([SugarScapeAgentOut])
                    -> ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut]
                     -> Bool
                     -> [SugarScapeAgentOut]
                     -> IO Bool
nextIteration outRef _ _ aouts = do
                                    writeIORef outRef aouts
                                    return False

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRenderWithTime :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut]
                                -> IORef [SugarScapeAgentOut]
                                -> IO ()
simulateAndRenderWithTime hdl outRef = simulateIO (Renderer.display winTitle winSize )
                                           GLO.white
                                           1
                                           []
                                           modelToPicture
                                           (nextFrameSimulateWithTime hdl outRef)

nextFrameSimulateWithTime :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut]
                                -> IORef [SugarScapeAgentOut]
                                -> ViewPort
                                -> Float
                                -> [SugarScapeAgentOut]
                                -> IO [SugarScapeAgentOut]
nextFrameSimulateWithTime hdl outRef _ _ outs = do
                                                 react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                                 aouts <- readIORef outRef
                                                 return aouts


simulateAndRenderNoTime :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut]
                        -> IORef ([SugarScapeAgentOut])
                        -> IO ()
simulateAndRenderNoTime hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrameSimulateNoTime hdl outRef)
                                            (\controller -> return () )

nextFrameSimulateNoTime :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut]
                -> IORef ([SugarScapeAgentOut])
                -> Float
                -> IO Picture
nextFrameSimulateNoTime hdl outRef dt = do
                                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                            aouts <- readIORef outRef
                                            modelToPicture aouts

modelToPicture :: [SugarScapeAgentOut] -> IO GLO.Picture
modelToPicture aouts
    | null aouts = return GLO.Blank
    | otherwise = do
                    -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
                    let env = aoEnv $ last aouts
                    return $ (Renderer.renderFrame aouts env winSize)
