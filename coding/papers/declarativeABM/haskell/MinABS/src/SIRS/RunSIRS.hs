module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsPar as PA

import System.Random

runSIRS :: IO ()
runSIRS = do
            --hSetBuffering stdin NoBuffering
            let dt = 1.0
            let dims = (50, 50)
            let initInfectionProb = 0.2
            let rngSeed = 42
            let steps = 10
            let g = mkStdGen rngSeed
            let (as, g') = createRandomSIRSAgents g dims initInfectionProb
            let env = sirsEnvironmentFromAgents as
            let hdl = PA.initStepSimulation as env
            stepWithRendering dims hdl dt

stepWithRendering :: (Int, Int) -> SIRSSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display "SIRS" (800, 800))
                                GLO.white
                                20
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
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)
--------------------------------------------------------------------------------------------------------------------------------------------------


--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
stepSIRS :: IO ()
stepSIRS = do
            --hSetBuffering stdin NoBuffering
            let dt = 1.0
            let dims = (30, 30)
            let initInfectionProb = 0.2
            let rngSeed = 42
            let steps = 10
            let g = mkStdGen rngSeed
            -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
            let (as, g') = createRandomSIRSAgents g dims initInfectionProb
            putStrLn "Initial:"
            printAgents as
            -- NOTE: this works for now when NOT using parallelism
            --  (as', e') <- atomically $ Agents.stepSimulation as Nothing dt steps
            --Agents.runSimulation as Nothing (outputStep dt)
            let env = sirsEnvironmentFromAgents as
            let hdl = PA.initStepSimulation as env
            runSteps hdl 100 dt
            return ()

runSteps :: SIRSSimHandle -> Int -> Double -> IO SIRSSimHandle
runSteps hdl 0 dt = return hdl
runSteps hdl n dt = do
                    let hdl' = PA.advanceSimulation hdl dt
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