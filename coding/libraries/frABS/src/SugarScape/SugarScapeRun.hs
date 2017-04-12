module SugarScape.SugarScapeRun where

import FrABS.Simulation.Simulation

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

winSize = (800, 800)
winTitle = "SugarScape Chapter III"
renderCircles = True

rngSeed = 42
agentCount = 400
envSize = (50, 50)

parallelStrategyFlag = False -- NOTE: sugarscape will not give correct result when run with parallel update-strategy

runSugarScapeWithRendering :: IO ()
runSugarScapeWithRendering = do
                                hSetBuffering stdout NoBuffering
                                hSetBuffering stderr NoBuffering
                                initRng rngSeed
                                (as, env) <- createSugarScape agentCount envSize

                                outRef <- (newIORef ([], env)) :: (IO (IORef ([SugarScapeAgentOut], SugarScapeEnvironment)))
                                hdl <- processIOInit as env parallelStrategyFlag (nextIteration outRef)

                                simulateAndRenderNoTime hdl outRef
                                -- simulateAndRenderWithTime env hdl outRef

nextIteration :: IORef ([SugarScapeAgentOut], SugarScapeEnvironment)
                    -> ReactHandle ([SugarScapeAgentIn], SugarScapeEnvironment) ([SugarScapeAgentOut], SugarScapeEnvironment)
                     -> Bool
                     -> ([SugarScapeAgentOut], SugarScapeEnvironment)
                     -> IO Bool
nextIteration outRef _ _ aep@(aouts, _) = do
                                                writeIORef outRef aep
                                                putStrLn ("" ++ (show $ length aouts))
                                                return False

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRenderWithTime :: SugarScapeEnvironment
                                -> ReactHandle ([SugarScapeAgentIn], SugarScapeEnvironment) ([SugarScapeAgentOut], SugarScapeEnvironment)
                                -> IORef ([SugarScapeAgentOut], SugarScapeEnvironment)
                                -> IO ()
simulateAndRenderWithTime initEnv hdl outRef = simulateIO (Renderer.display winTitle winSize )
                                                   GLO.white
                                                   1
                                                   ([], initEnv)
                                                   modelToPicture
                                                   (nextFrameSimulateWithTime hdl outRef)

nextFrameSimulateWithTime :: ReactHandle ([SugarScapeAgentIn], SugarScapeEnvironment) ([SugarScapeAgentOut], SugarScapeEnvironment)
                                -> IORef ([SugarScapeAgentOut], SugarScapeEnvironment)
                                -> ViewPort
                                -> Float
                                -> ([SugarScapeAgentOut], SugarScapeEnvironment)
                                -> IO ([SugarScapeAgentOut], SugarScapeEnvironment)
nextFrameSimulateWithTime hdl outRef _ _ _ = do
                                                 react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                                 aouts <- readIORef outRef
                                                 return aouts


simulateAndRenderNoTime :: ReactHandle ([SugarScapeAgentIn], SugarScapeEnvironment) ([SugarScapeAgentOut], SugarScapeEnvironment)
                        -> IORef ([SugarScapeAgentOut], SugarScapeEnvironment)
                        -> IO ()
simulateAndRenderNoTime hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrameSimulateNoTime hdl outRef)
                                            (\_ -> return () )

nextFrameSimulateNoTime :: ReactHandle ([SugarScapeAgentIn], SugarScapeEnvironment) ([SugarScapeAgentOut], SugarScapeEnvironment)
                -> IORef ([SugarScapeAgentOut], SugarScapeEnvironment)
                -> Float
                -> IO Picture
nextFrameSimulateNoTime hdl outRef _ = do
                                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                            aouts <- readIORef outRef
                                            modelToPicture aouts

-- TODO: when no agents are there then the environment will disappear, need to communicate the environment as a separate output
modelToPicture :: ([SugarScapeAgentOut], SugarScapeEnvironment) -> IO GLO.Picture
modelToPicture (aouts, env) = do
                                -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
                                return $ (Renderer.renderFrame aouts env winSize)
