module Wildfire.WildfireRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import Wildfire.WildfireModel
import Wildfire.WildfireInit
import Wildfire.WildfireRenderer as Renderer

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import Data.IORef
import System.IO
import System.Random
import Control.Monad.Random

winSize = (800, 800)
winTitle = "Wildfire"

rngSeed = 42
envSize = (35, 35)
samplingTimeDelta = 0.2

runWildfireWithRendering :: IO ()
runWildfireWithRendering = 
    do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering
        initRng rngSeed
        (adefs, env) <- initWildfire envSize
        params <- simParams

        outRef <- (newIORef ([], env)) :: (IO (IORef ([WildfireAgentOut], WildfireEnvironment)))
        hdl <- processIOInit adefs env params (nextIteration outRef)

        simulateAndRenderNoTime hdl outRef
        --simulateAndRenderWithTime env hdl outRef

nextIteration :: IORef ([WildfireAgentOut], WildfireEnvironment)
                    -> ReactHandle ([WildfireAgentIn], WildfireEnvironment) ([WildfireAgentOut], WildfireEnvironment)
                     -> Bool
                     -> ([WildfireAgentOut], WildfireEnvironment)
                     -> IO Bool
nextIteration outRef _ _ aep@(aouts, _) = 
    do
        writeIORef outRef aep
        --putStrLn ("" ++ (show $ length aouts))
        --mapM printAgent aouts
        return False

simParams :: IO (SimulationParams WildfireCell WildfireLinkLabel)
simParams = 
    do
        rng <- getSplit
        return SimulationParams {
            simStrategy = Parallel,   -- NOTE: wildfire should work both for parallel and sequential
            simEnvCollapse = Nothing,
            simShuffleAgents = False,   -- NOTE: don't forget to shuffle the agents in case of sequential
            simRng = rng
        }

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g

simulateAndRenderWithTime :: WildfireEnvironment
                                -> ReactHandle ([WildfireAgentIn], WildfireEnvironment) ([WildfireAgentOut], WildfireEnvironment)
                                -> IORef ([WildfireAgentOut], WildfireEnvironment)
                                -> IO ()
simulateAndRenderWithTime initEnv hdl outRef = simulateIO (Renderer.display winTitle winSize )
                                                   GLO.white
                                                   1
                                                   ([], initEnv)
                                                   modelToPicture
                                                   (nextFrameSimulateWithTime hdl outRef)

nextFrameSimulateWithTime :: ReactHandle ([WildfireAgentIn], WildfireEnvironment) ([WildfireAgentOut], WildfireEnvironment)
                                -> IORef ([WildfireAgentOut], WildfireEnvironment)
                                -> ViewPort
                                -> Float
                                -> ([WildfireAgentOut], WildfireEnvironment)
                                -> IO ([WildfireAgentOut], WildfireEnvironment)
nextFrameSimulateWithTime hdl outRef _ _ _ = 
    do
        react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        return aouts


simulateAndRenderNoTime :: ReactHandle ([WildfireAgentIn], WildfireEnvironment) ([WildfireAgentOut], WildfireEnvironment)
                        -> IORef ([WildfireAgentOut], WildfireEnvironment)
                        -> IO ()
simulateAndRenderNoTime hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrameSimulateNoTime hdl outRef)
                                            (\_ -> return () )

nextFrameSimulateNoTime :: ReactHandle ([WildfireAgentIn], WildfireEnvironment) ([WildfireAgentOut], WildfireEnvironment)
                -> IORef ([WildfireAgentOut], WildfireEnvironment)
                -> Float
                -> IO Picture
nextFrameSimulateNoTime hdl outRef _ = 
    do
        react hdl (samplingTimeDelta, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        modelToPicture aouts

-- TODO: when no agents are there then the environment will disappear, need to communicate the environment as a separate output
modelToPicture :: ([WildfireAgentOut], WildfireEnvironment) -> IO GLO.Picture
modelToPicture (aouts, env) = 
    do
        -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
        return $ (Renderer.renderFrame aouts env winSize)
