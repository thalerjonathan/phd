module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified FrABS.Rendering.Agents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Data.IORef
import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FRP.Yampa
import System.IO
import System.Random

winSize = (800, 800)
winTitle = "SIRS FrABS"

rngSeed = 42
cells = (51, 51)
initInfectionProb = 0.2
parallelStrategy = Nothing

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
                        hSetBuffering stdin NoBuffering
                        initRng rngSeed
                        as <- createRandomSIRSAgents cells initInfectionProb
                        env <- createSIRSEnv cells as

                        outRef <- (newIORef ([], env)) :: (IO (IORef ([SIRSAgentOut], SIRSEnvironment)))
                        hdl <- processIOInit as env parallelStrategy (nextIteration outRef)

                        simulateAndRender hdl outRef

nextIteration :: IORef ([SIRSAgentOut], SIRSEnvironment)
                    -> ReactHandle ([SIRSAgentIn], SIRSEnvironment) ([SIRSAgentOut], SIRSEnvironment)
                     -> Bool
                     -> ([SIRSAgentOut], SIRSEnvironment)
                     -> IO Bool
nextIteration outRef _ _ aouts = do
                                    --putStrLn "nextIteration: writing Ref"
                                    writeIORef outRef aouts
                                    return False


runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender = do
                            hSetBuffering stdin NoBuffering
                            initRng rngSeed
                            as <- createRandomSIRSAgents cells initInfectionProb
                            env <- createSIRSEnv cells as

                            let steps = 10
                            let ass = processSteps as env parallelStrategy 1.0 steps
                            let (as', _) = last ass

                            pic <- modelToPicture as'
                            GLO.display (Front.display winTitle winSize) GLO.white pic
                            --mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRender :: ReactHandle ([SIRSAgentIn], SIRSEnvironment) ([SIRSAgentOut], SIRSEnvironment)
                        -> IORef ([SIRSAgentOut], SIRSEnvironment) -> IO ()
simulateAndRender hdl outRef = animateIO (Front.display winTitle winSize)
                                            GLO.white
                                            (nextFrame hdl outRef)
                                            (\_ -> return () )

nextFrame :: ReactHandle ([SIRSAgentIn], SIRSEnvironment) ([SIRSAgentOut], SIRSEnvironment)
                -> IORef ([SIRSAgentOut], SIRSEnvironment)
                -> Float
                -> IO Picture
nextFrame hdl outRef _ = do
                            --putStrLn "nextFrame: before react"
                            react hdl (1.0, Nothing) -- NOTE: will result in call to nextIteration
                            --putStrLn "nextFrame: after react"
                            (aouts, _) <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: [SIRSAgentOut] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map sirsAgentToRenderCell as
                        return (Front.renderFrame True rcs winSize cells)

sirsAgentToRenderCell :: SIRSAgentOut -> Front.RenderCell
sirsAgentToRenderCell a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = aoState a
        (ax, ay) = (sirsCoord s)
        ss = case (sirsState s) of
                        Susceptible -> (0.0, 0.55, 0.0)
                        Infected -> (0.7, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 0.7)