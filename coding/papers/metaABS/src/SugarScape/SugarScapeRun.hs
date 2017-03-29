module SugarScape.SugarScapeRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import SugarScape.SugarScapeModel
import SugarScape.SugarScapeInit
import SugarScape.SugarScapeRenderer as Renderer

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate

import Data.IORef
import System.IO
import System.Random

winSize = (800, 800)
winTitle = "SugarScape Chapter 2 FrABS"
renderCircles = True

rngSeed = 42
agentCount = 5
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

                                simulateAndRender hdl outRef

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

simulateAndRender :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut] -> IORef ([SugarScapeAgentOut]) -> IO ()
simulateAndRender hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrame hdl outRef)
                                            (\controller -> return () )

nextFrame :: ReactHandle [SugarScapeAgentIn] [SugarScapeAgentOut] -> IORef ([SugarScapeAgentOut]) -> Float -> IO Picture
nextFrame hdl outRef dt = do
                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                            aouts <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: [SugarScapeAgentOut] -> IO GLO.Picture
modelToPicture aouts = do
                        -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
                        let env = aoEnv $ last aouts
                        return (Renderer.renderFrame aouts env winSize)
