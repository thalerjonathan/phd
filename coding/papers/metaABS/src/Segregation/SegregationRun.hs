module Segregation.SegregationRun where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation

import Segregation.SegregationModel
import Segregation.SegregationInit
import Segregation.SegregationStats

import FRP.Yampa
import qualified FrABS.Rendering.Agents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate

import Data.IORef
import System.IO
import System.Random

winSize = (800, 800)
winTitle = "Schelling Segregation FrABS"
renderCircles = True

rngSeed = 42
cells = (50, 50)
parallelStrategyFlag = False -- NOTE: segregation will not give correct result when run with parallel update-strategy


runSegWithRendering :: IO ()
runSegWithRendering = do
                        hSetBuffering stdout NoBuffering
                        hSetBuffering stderr NoBuffering
                        initRng rngSeed
                        (as, env) <- createSegAgentsAndEnv cells

                        putStrLn "dynamics = ["
                        outRef <- (newIORef ([], False)) :: (IO (IORef ([SegAgentOut], Bool)))
                        hdl <- processIOInit as env parallelStrategyFlag (nextIteration outRef)

                        simulateAndRender hdl outRef

nextIteration :: IORef ([SegAgentOut], Bool)
                    -> ReactHandle [AgentIn s m ec] [SegAgentOut]
                     -> Bool
                     -> [SegAgentOut]
                     -> IO Bool
nextIteration outRef _ _ aouts = do
                                    printDynamics outRef aouts
                                    let allSatisfied = all isSatisfied aouts
                                    writeIORef outRef (aouts, allSatisfied)
                                    return allSatisfied

printDynamics :: IORef ([SegAgentOut], Bool) -> [SegAgentOut] -> IO ()
printDynamics outRef aoutsCurr = do
                                    (aoutsPrev, _) <- readIORef outRef

                                    let maxSimilarity = fromInteger $ fromIntegral totalCount -- NOTE: an agent can reach a maximum of 1.0
                                    let currSimilarity = totalSatisfaction aoutsCurr
                                    let prevSimilarity = totalSatisfaction aoutsPrev
                                    let similarityDelta = currSimilarity - prevSimilarity

                                    let currSimilarityNormalized = currSimilarity / maxSimilarity
                                    let similarityDeltaNormalized = similarityDelta / maxSimilarity

                                    putStrLn (show unhappyFract
                                                ++ "," ++ show currSimilarityNormalized
                                                ++ "," ++ show similarityDeltaNormalized
                                                ++ ";" )
                                    where
                                        (totalCount, happyCount, unhappyCount, unhappyFract) = satisfactionStats aoutsCurr

runSegStepsAndRender :: IO ()
runSegStepsAndRender = do
                            hSetBuffering stdout NoBuffering
                            initRng rngSeed
                            (as, env) <- createSegAgentsAndEnv cells

                            let steps = 10
                            let ass = processSteps as env parallelStrategyFlag 1.0 steps
                            let as' = last ass

                            --pic <- modelToPicture as'
                            --GLO.display (Front.display winTitle winSize) GLO.black pic
                            mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRender :: ReactHandle [AgentIn s m ec] [SegAgentOut] -> IORef ([SegAgentOut], Bool) -> IO ()
simulateAndRender hdl outRef = animateIO (Front.display winTitle winSize)
                                            GLO.black -- GLO.white
                                            (nextFrame hdl outRef)
                                            (\controller -> return () )

nextFrame :: ReactHandle [AgentIn s m ec] [SegAgentOut] -> IORef ([SegAgentOut], Bool) -> Float -> IO Picture
nextFrame hdl outRef dt = do
                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                            (aouts, _) <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: [SegAgentOut] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map (segAgentOutToRenderCell cells) as
                        return (Front.renderFrame renderCircles rcs winSize cells)

segAgentOutToRenderCell :: (Int, Int) -> SegAgentOut -> Front.RenderCell
segAgentOutToRenderCell (xDim, yDim) ao = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = col }
    where
        s = aoState ao
        (ax, ay) = (aoEnvPos ao)
        col = case (segParty s) of
                        Red -> (0.6, 0.0, 0.0)
                        Green -> (0.0, 0.6, 0.0)