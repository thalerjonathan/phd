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
cells = (10, 10)
parallelStrategy = Nothing -- NOTE: segregation will not give correct result when run with parallel update-strategy


runSegWithRendering :: IO ()
runSegWithRendering = do
                        hSetBuffering stdout NoBuffering
                        hSetBuffering stderr NoBuffering
                        initRng rngSeed
                        (as, env) <- createSegAgentsAndEnv cells

                        putStrLn "dynamics = ["
                        outRef <- (newIORef (([], env), False)) :: (IO (IORef (([SegAgentOut], SegEnvironment), Bool)))
                        hdl <- processIOInit as env parallelStrategy (nextIteration outRef)

                        simulateAndRender hdl outRef

nextIteration :: IORef (([SegAgentOut], SegEnvironment), Bool)
                    -> ReactHandle ([SegAgentIn], SegEnvironment) ([SegAgentOut], SegEnvironment)
                     -> Bool
                     -> ([SegAgentOut], SegEnvironment)
                     -> IO Bool
nextIteration outRef _ _ aep@(aouts, _) = do
                                            printDynamics outRef aouts
                                            let allSatisfied = all isSatisfied aouts
                                            writeIORef outRef (aep, allSatisfied)
                                            return allSatisfied

printDynamics :: IORef (([SegAgentOut], SegEnvironment), Bool)
                    -> [SegAgentOut]
                    -> IO ()
printDynamics outRef aoutsCurr = do
                                    ((aoutsPrev, _), _) <- readIORef outRef

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
                                        (totalCount, _, _, unhappyFract) = satisfactionStats aoutsCurr

runSegStepsAndRender :: IO ()
runSegStepsAndRender = do
                            hSetBuffering stdout NoBuffering
                            initRng rngSeed
                            (as, env) <- createSegAgentsAndEnv cells

                            let steps = 10
                            let ass = processSteps as env parallelStrategy 1.0 steps
                            let (as', _) = last ass

                            --pic <- modelToPicture as'
                            --GLO.display (Front.display winTitle winSize) GLO.black pic
                            mapM (putStrLn . show . aoState) as'

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

simulateAndRender :: ReactHandle ([SegAgentIn], SegEnvironment) ([SegAgentOut], SegEnvironment)
                        -> IORef (([SegAgentOut], SegEnvironment), Bool) -> IO ()
simulateAndRender hdl outRef = animateIO (Front.display winTitle winSize)
                                            GLO.black -- GLO.white
                                            (nextFrame hdl outRef)
                                            (\_ -> return () )

nextFrame :: ReactHandle ([SegAgentIn], SegEnvironment) ([SegAgentOut], SegEnvironment)
                -> IORef (([SegAgentOut], SegEnvironment), Bool) -> Float -> IO Picture
nextFrame hdl outRef _ = do
                            react hdl (1.0, Nothing)  -- NOTE: will result in call to nextIteration
                            (aouts, _) <- readIORef outRef
                            modelToPicture aouts

modelToPicture :: ([SegAgentOut], SegEnvironment) -> IO GLO.Picture
modelToPicture (as, _) = do
                            let rcs = map segAgentOutToRenderCell as
                            return (Front.renderFrame renderCircles rcs winSize cells)

segAgentOutToRenderCell :: SegAgentOut -> Front.RenderCell
segAgentOutToRenderCell ao = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = col }
    where
        s = aoState ao
        (ax, ay) = (aoEnvPos ao)
        col = case (segParty s) of
                        Red -> (0.6, 0.0, 0.0)
                        Green -> (0.0, 0.6, 0.0)