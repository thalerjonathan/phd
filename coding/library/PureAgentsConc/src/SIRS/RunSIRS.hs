module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsConc as PA

import System.Random
import Control.Monad.STM

runSIRSRendering :: IO ()
runSIRSRendering = do
                    --hSetBuffering stdin NoBuffering
                    let dt = 1.0
                    let dims = (50, 50)
                    let initInfectionProb = 0.2
                    let rngSeed = 42
                    let g = mkStdGen rngSeed
                    (as, g') <- atomically $ createRandomSIRSAgents g dims initInfectionProb
                    hdl <- atomically $ PA.initStepSimulation as ()
                    stepWithRendering dims hdl dt

runSIRSSteps :: IO ()
runSIRSSteps = do
                --hSetBuffering stdin NoBuffering
                let dt = 1.0
                let dims = (50, 50)
                let initInfectionProb = 0.2
                let rngSeed = 42
                let steps = 1000
                let g = mkStdGen rngSeed
                (as, g') <- atomically $ createRandomSIRSAgents g dims initInfectionProb
                let stepCount = 10
                (as', _) <- PA.stepSimulation as () dt stepCount
                mapM (putStrLn . show . PA.state) as'
                return ()

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender = do
                            --hSetBuffering stdin NoBuffering
                            let dt = 1.0
                            let dims = (50, 50)
                            let initInfectionProb = 0.2
                            let rngSeed = 42
                            let steps = 1000
                            let g = mkStdGen rngSeed
                            (as, g') <- atomically $ createRandomSIRSAgents g dims initInfectionProb
                            let stepCount = 10
                            (as', _) <- PA.stepSimulation as () dt stepCount
                            let cells = map (sirsAgentToRenderCell dims) as'
                            let frameRender = (Front.renderFrame cells (800, 800) dims)
                            GLO.display (Front.display "SIRS" (800, 800)) GLO.white frameRender
                            return ()

stepWithRendering :: (Int, Int) -> SIRSSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display "SIRS" (800, 800))
                                GLO.white
                                1
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> SIRSSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            let as = PA.extractHdlAgents hdl
                            let cells = map (sirsAgentToRenderCell dims) as
                            return (Front.renderFrame cells (800, 800) dims)

sirsAgentToRenderCell :: (Int, Int) -> SIRSAgent -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        id = PA.agentId a
        s = PA.state a
        ax = mod id yDim
        ay = floor((fromIntegral id) / (fromIntegral xDim))
        ss = case (sirState s) of
                        Susceptible -> (0.0, 1.0, 0.0)
                        Infected -> (1.0, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 1.0)

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> SIRSSimHandle -> IO SIRSSimHandle
stepIteration fixedDt viewport dtRendering hdl = PA.advanceSimulation hdl fixedDt
--------------------------------------------------------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
stepSIRS :: IO ()
stepSIRS = do
            --hSetBuffering stdin NoBuffering
            let dt = 1.0
            let dims = (100, 100)
            let initInfectionProb = 0.2
            let rngSeed = 42
            let steps = 100
            let g = mkStdGen rngSeed
            -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
            (as, g') <- atomically $ createRandomSIRSAgents g dims initInfectionProb
            putStrLn "Initial:"
            printAgents as
            -- NOTE: this works for now when NOT using parallelism
            --  (as', e') <- atomically $ Agents.stepSimulation as Nothing dt steps
            --Agents.runSimulation as Nothing (outputStep dt)
            hdl <- atomically $ PA.initStepSimulation as ()
            runSteps hdl 100 dt
            return ()

runSteps :: SIRSSimHandle -> Int -> Double -> IO SIRSSimHandle
runSteps hdl 0 dt = return hdl
runSteps hdl n dt = do
                    hdl' <- PA.advanceSimulation hdl dt
                    let as = PA.extractHdlAgents hdl'
                    putStrLn ("Step " ++ (show n) ++ ":")
                    printAgents as
                    runSteps hdl' (n-1) dt

outputStep :: (Show e) => Double -> (([SIRSAgent], Maybe e) -> IO (Bool, Double))
outputStep dt (as, e) = do
                            c <- getLine
                            putStrLn c
                            putStrLn (show e)
                            printAgents as
                            return (True, dt)

printAgents :: [SIRSAgent] -> IO ()
printAgents as = do
                    mapM (putStrLn . show . PA.state) as
                    return ()
--------------------------------------------------------------------------------------------------------------------------------------------------