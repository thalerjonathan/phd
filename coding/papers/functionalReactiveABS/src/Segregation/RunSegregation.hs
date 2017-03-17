module Segregation.RunSegregation where

import Segregation.Segregation
import qualified FrABS.Agents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate
import Data.IORef
import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FRP.Yampa
import System.IO
import System.Random

winSize = (800, 800)
winTitle = "Schelling Segregation FrABS"

rngSeed = 42
cells = (51, 51)
parallelStrategyFlag = False

runSegWithRendering :: IO ()
runSegWithRendering = do
                        hSetBuffering stdin NoBuffering
                        initRng rngSeed
                        as <- createRandomSegAgents cells

                        outRef <- (newIORef []) :: (IO (IORef [SegAgentOut]))
                        hdl <- processIOInit as () parallelStrategyFlag (nextIteration outRef)

                        simulateAndRender hdl outRef

nextIteration :: IORef [SegAgentOut]
                    -> ReactHandle [AgentIn s m ec] [SegAgentOut]
                     -> Bool
                     -> [SegAgentOut]
                     -> IO Bool
nextIteration outRef _ _ aouts = do
                                    --putStrLn "nextIteration: writing Ref"
                                    writeIORef outRef aouts
                                    return False


runSegStepsAndRender :: IO ()
runSegStepsAndRender = do
                            hSetBuffering stdin NoBuffering
                            initRng rngSeed
                            as <- createRandomSegAgents cells

                            let steps = 10
                            let ass = processSteps as () parallelStrategyFlag 1.0 steps
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

simulateAndRender :: ReactHandle [AgentIn s m ec] [SegAgentOut] -> IORef [SegAgentOut] -> IO ()
simulateAndRender hdl outRef = animateIO (Front.display winTitle winSize)
                                            GLO.white
                                            (nextFrame hdl outRef)
                                            (\controller -> return () )

nextFrame :: ReactHandle [AgentIn s m ec] [SegAgentOut] -> IORef [SegAgentOut] -> Float -> IO Picture
nextFrame hdl outRef dt = do
                            --putStrLn "nextFrame: before react"
                            react hdl (1.0, Nothing) -- NOTE: will result in call to nextIteration
                            --putStrLn "nextFrame: after react"
                            aouts <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: [SegAgentOut] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map (sirsAgentToRenderCell cells) as
                        return (Front.renderFrame rcs winSize cells)

sirsAgentToRenderCell :: (Int, Int) -> SegAgentOut -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = aoState a
        (ax, ay) = (segCoord s)
        ss = case (segAgentType s) of
                        Red -> (0.0, 0.55, 0.0)
                        Green -> (0.7, 0.0, 0.0)