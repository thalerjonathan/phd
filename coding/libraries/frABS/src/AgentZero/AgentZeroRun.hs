module AgentZero.AgentZeroRun where

import FrABS.Simulation.Simulation

import AgentZero.AgentZeroModel
import AgentZero.AgentZeroInit
import AgentZero.AgentZeroEnvironment
import AgentZero.AgentZeroRenderer as Renderer

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import Data.IORef
import System.IO
import System.Random

winSize = (800, 800)
winTitle = "Agent_Zero"

rngSeed = 42
agentCount = 30
envSize = (50, 50)

parallelStrategy = Nothing -- Just agentZeroEnvironmentsCollapse -- NOTE: agent-zero works BOTH for parallel and sequential

runAgentZeroWithRendering :: IO ()
runAgentZeroWithRendering = do
                                hSetBuffering stdout NoBuffering
                                hSetBuffering stderr NoBuffering
                                initRng rngSeed
                                (as, env) <- createAgentZero agentCount envSize

                                outRef <- (newIORef ([], env)) :: (IO (IORef ([AgentZeroAgentOut], AgentZeroEnvironment)))
                                hdl <- processIOInit as env parallelStrategy (nextIteration outRef)

                                simulateAndRenderNoTime hdl outRef
                                -- simulateAndRenderWithTime env hdl outRef

nextIteration :: IORef ([AgentZeroAgentOut], AgentZeroEnvironment)
                    -> ReactHandle ([AgentZeroAgentIn], AgentZeroEnvironment) ([AgentZeroAgentOut], AgentZeroEnvironment)
                     -> Bool
                     -> ([AgentZeroAgentOut], AgentZeroEnvironment)
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

simulateAndRenderWithTime :: AgentZeroEnvironment
                                -> ReactHandle ([AgentZeroAgentIn], AgentZeroEnvironment) ([AgentZeroAgentOut], AgentZeroEnvironment)
                                -> IORef ([AgentZeroAgentOut], AgentZeroEnvironment)
                                -> IO ()
simulateAndRenderWithTime initEnv hdl outRef = simulateIO (Renderer.display winTitle winSize )
                                                   GLO.white
                                                   1
                                                   ([], initEnv)
                                                   modelToPicture
                                                   (nextFrameSimulateWithTime hdl outRef)

nextFrameSimulateWithTime :: ReactHandle ([AgentZeroAgentIn], AgentZeroEnvironment) ([AgentZeroAgentOut], AgentZeroEnvironment)
                                -> IORef ([AgentZeroAgentOut], AgentZeroEnvironment)
                                -> ViewPort
                                -> Float
                                -> ([AgentZeroAgentOut], AgentZeroEnvironment)
                                -> IO ([AgentZeroAgentOut], AgentZeroEnvironment)
nextFrameSimulateWithTime hdl outRef _ _ _ = do
                                                 react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                                 aouts <- readIORef outRef
                                                 return aouts


simulateAndRenderNoTime :: ReactHandle ([AgentZeroAgentIn], AgentZeroEnvironment) ([AgentZeroAgentOut], AgentZeroEnvironment)
                        -> IORef ([AgentZeroAgentOut], AgentZeroEnvironment)
                        -> IO ()
simulateAndRenderNoTime hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrameSimulateNoTime hdl outRef)
                                            (\_ -> return () )

nextFrameSimulateNoTime :: ReactHandle ([AgentZeroAgentIn], AgentZeroEnvironment) ([AgentZeroAgentOut], AgentZeroEnvironment)
                -> IORef ([AgentZeroAgentOut], AgentZeroEnvironment)
                -> Float
                -> IO Picture
nextFrameSimulateNoTime hdl outRef _ = do
                                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                                            aouts <- readIORef outRef
                                            modelToPicture aouts

-- TODO: when no agents are there then the environment will disappear, need to communicate the environment as a separate output
modelToPicture :: ([AgentZeroAgentOut], AgentZeroEnvironment) -> IO GLO.Picture
modelToPicture (aouts, env) = do
                                -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
                                return $ (Renderer.renderFrame aouts env winSize)
