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
parallelStrategyFlag = False

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
                        hSetBuffering stdin NoBuffering
                        initRng rngSeed
                        as <- createRandomSIRSAgents cells initInfectionProb
                        let env = createSIRSEnv cells as

                        outRef <- (newIORef []) :: (IO (IORef [SIRSAgentOut]))
                        hdl <- processIOInit as env parallelStrategyFlag (nextIteration outRef)

                        simulateAndRender hdl outRef

nextIteration :: IORef [SIRSAgentOut]
                    -> ReactHandle [AgentIn s m ec] [SIRSAgentOut]
                     -> Bool
                     -> [SIRSAgentOut]
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
                            let env = createSIRSEnv cells as

                            let steps = 10
                            let ass = processSteps as env parallelStrategyFlag 1.0 steps
                            let as' = last ass

                            pic <- modelToPicture as'
                            GLO.display (Front.display winTitle winSize) GLO.white pic
                            --mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRender :: ReactHandle [AgentIn s m ec] [SIRSAgentOut] -> IORef [SIRSAgentOut] -> IO ()
simulateAndRender hdl outRef = animateIO (Front.display winTitle winSize)
                                            GLO.white
                                            (nextFrame hdl outRef)
                                            (\controller -> return () )

nextFrame :: ReactHandle [AgentIn s m ec] [SIRSAgentOut]
                -> IORef [SIRSAgentOut]
                -> Float
                -> IO Picture
nextFrame hdl outRef dt = do
                            --putStrLn "nextFrame: before react"
                            react hdl (1.0, Nothing) -- NOTE: will result in call to nextIteration
                            --putStrLn "nextFrame: after react"
                            aouts <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: [SIRSAgentOut] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map (sirsAgentToRenderCell cells) as
                        return (Front.renderFrame True rcs winSize cells)

sirsAgentToRenderCell :: (Int, Int) -> SIRSAgentOut -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = aoState a
        (ax, ay) = (sirsCoord s)
        ss = case (sirsState s) of
                        Susceptible -> (0.0, 0.55, 0.0)
                        Infected -> (0.7, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 0.7)