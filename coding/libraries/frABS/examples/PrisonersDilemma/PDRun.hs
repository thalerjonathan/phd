module PrisonersDilemma.PDRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import PrisonersDilemma.PDModel
import PrisonersDilemma.PDInit
import PrisonersDilemma.PDRenderer as Renderer

import FRP.Yampa
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Graphics.Gloss.Interface.IO.Simulate

import Data.IORef
import System.IO
import System.Random
import Control.Monad.Random

winSize = (800, 800)
winTitle = "Prisoners Dilemma"

rngSeed = 42
envSize = (49, 49)
samplingTimeDelta = 0.2

runPDWithRendering :: IO ()
runPDWithRendering = 
    do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering
        initRng rngSeed
        (adefs, env) <- initPrisonersDilemma envSize
        params <- simParams

        outRef <- (newIORef ([], env)) :: (IO (IORef ([PDAgentOut], PDEnvironment)))
        hdl <- processIOInit adefs env params (nextIteration outRef)

        simulateAndRenderNoTime hdl outRef
        --simulateAndRenderWithTime env hdl outRef

nextIteration :: IORef ([PDAgentOut], PDEnvironment)
                    -> ReactHandle ([PDAgentIn], PDEnvironment) ([PDAgentOut], PDEnvironment)
                     -> Bool
                     -> ([PDAgentOut], PDEnvironment)
                     -> IO Bool
nextIteration outRef _ _ aep@(aouts, _) = 
    do
        writeIORef outRef aep
        --putStrLn ("" ++ (show $ length aouts))
        --mapM printAgent aouts
        return False

simParams :: IO (SimulationParams PDCell PDLinkLabel)
simParams = 
    do
        rng <- getSplit
        return SimulationParams {
            simStrategy = Parallel,
            simEnvCollapse = Nothing,
            simShuffleAgents = False,
            simRng = rng
        }

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g

simulateAndRenderWithTime :: PDEnvironment
                                -> ReactHandle ([PDAgentIn], PDEnvironment) ([PDAgentOut], PDEnvironment)
                                -> IORef ([PDAgentOut], PDEnvironment)
                                -> IO ()
simulateAndRenderWithTime initEnv hdl outRef = simulateIO (Renderer.display winTitle winSize )
                                                   GLO.white
                                                   1
                                                   ([], initEnv)
                                                   modelToPicture
                                                   (nextFrameSimulateWithTime hdl outRef)

nextFrameSimulateWithTime :: ReactHandle ([PDAgentIn], PDEnvironment) ([PDAgentOut], PDEnvironment)
                                -> IORef ([PDAgentOut], PDEnvironment)
                                -> ViewPort
                                -> Float
                                -> ([PDAgentOut], PDEnvironment)
                                -> IO ([PDAgentOut], PDEnvironment)
nextFrameSimulateWithTime hdl outRef _ _ _ = 
    do
        react hdl (samplingTimeDelta, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        return aouts


simulateAndRenderNoTime :: ReactHandle ([PDAgentIn], PDEnvironment) ([PDAgentOut], PDEnvironment)
                        -> IORef ([PDAgentOut], PDEnvironment)
                        -> IO ()
simulateAndRenderNoTime hdl outRef = animateIO (Renderer.display winTitle winSize)
                                            GLO.white
                                            (nextFrameSimulateNoTime hdl outRef)
                                            (\_ -> return () )

nextFrameSimulateNoTime :: ReactHandle ([PDAgentIn], PDEnvironment) ([PDAgentOut], PDEnvironment)
                -> IORef ([PDAgentOut], PDEnvironment)
                -> Float
                -> IO Picture
nextFrameSimulateNoTime hdl outRef _ = 
    do
        react hdl (samplingTimeDelta, Nothing)  -- NOTE: will result in call to nextIteration
        aouts <- readIORef outRef
        modelToPicture aouts

-- TODO: when no agents are there then the environment will disappear, need to communicate the environment as a separate output
modelToPicture :: ([PDAgentOut], PDEnvironment) -> IO GLO.Picture
modelToPicture (aouts, env) = 
    do
        -- NOTE: the corresponding most recent environment is the one of the last agentout because for now we are iterating sequentially
        return $ (Renderer.renderFrame aouts env winSize)
