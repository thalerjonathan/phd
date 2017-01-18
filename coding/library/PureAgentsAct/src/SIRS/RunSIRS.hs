module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsAct as PA

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
                    hdl <- PA.startSimulation as dt ()
                    stepWithRendering dims hdl dt


stepWithRendering :: (Int, Int) -> SIRSSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display "SIRS ACT" (800, 800))
                                GLO.white
                                30
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> SIRSSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            as <- PA.observeAgentStates hdl
                            let cells = map (sirsAgentToRenderCell dims) as
                            return (Front.renderFrame cells (800, 800) dims)

sirsAgentToRenderCell :: (Int, Int) -> (PA.AgentId, Double, SIRSAgentState) -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) (aid, t, s) = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        ax = mod aid yDim
        ay = floor((fromIntegral aid) / (fromIntegral xDim))
        ss = case (sirState s) of
                        Susceptible -> (0.0, 1.0, 0.0)
                        Infected -> (1.0, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 1.0)

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> SIRSSimHandle -> IO SIRSSimHandle
stepIteration fixedDt viewport dtRendering hdl = return hdl
--------------------------------------------------------------------------------------------------------------------------------------------------