module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified GridRenderer as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified MinABS as ABS

import System.Random

winSize = (800, 800)
winTitle = "SIRS MinABS"

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
                        --hSetBuffering stdin NoBuffering
                        let dt = 1.0
                        let dims = (50, 50)
                        let initInfectionProb = 0.2
                        let rngSeed = 42
                        let steps = 10
                        let g = mkStdGen rngSeed
                        let (as, g') = createRandomSIRSAgents g dims initInfectionProb
                        let as' = ABS.iterationStart as
                        stepWithRendering dims as' dt

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender = do
                            --hSetBuffering stdin NoBuffering
                            let dt = 1.0
                            let dims = (50, 50)
                            let initInfectionProb = 0.0
                            let rngSeed = 42
                            let steps = 1
                            let g = mkStdGen rngSeed
                            let (as, g') = createRandomSIRSAgents g dims initInfectionProb

                            let as' = ABS.iterationSteps as steps

                            let observableAgentStates = map (sirsAgentToRenderCell dims) as'
                            let frameRender = (Front.renderFrame observableAgentStates winSize dims)
                            GLO.display (Front.display winTitle winSize) GLO.white frameRender
                            return ()


stepWithRendering :: (Int, Int) -> [SIRSAgent] -> Double -> IO ()
stepWithRendering dims as dt = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                as
                                (modelToPicture dims)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> [SIRSAgent] -> IO GLO.Picture
modelToPicture dims as = do
                            let cells = map (sirsAgentToRenderCell dims) as
                            return (Front.renderFrame cells winSize dims)

sirsAgentToRenderCell :: (Int, Int) -> SIRSAgent -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        id = ABS.aid a
        s = ABS.s a
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
stepIteration :: Double -> ViewPort -> Float -> [SIRSAgent] -> IO [SIRSAgent]
stepIteration fixedDt viewport dtRendering as = return (ABS.iterationNext as)
--------------------------------------------------------------------------------------------------------------------------------------------------