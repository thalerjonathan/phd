module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsConc as PA

import System.Random
import Control.Monad.STM

winTitle = "SIRS CONC"
winSize = (800, 800)

runSIRSRendering :: IO ()
runSIRSRendering = do
                    --hSetBuffering stdin NoBuffering
                    let dt = 1.0
                    let dims = (50, 50)
                    let initInfectionProb = 0.2
                    let rngSeed = 42
                    let g = mkStdGen rngSeed
                    (as, g') <- atomically $ createRandomSIRSAgents g dims initInfectionProb
                    hdl <- PA.initStepSimulation as ()
                    stepWithRendering dims hdl dt

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
                            as' <- PA.stepSimulation as () dt stepCount
                            let cells = map (sirsAgentToRenderCell dims) as'
                            let frameRender = (Front.renderFrame cells winSize dims)
                            GLO.display (Front.display winTitle winSize) GLO.white frameRender
                            return ()

stepWithRendering :: (Int, Int) -> SIRSSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

modelToPicture :: (Int, Int) -> SIRSSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            let as = PA.extractHdlAgents hdl
                            let cells = map (sirsAgentToRenderCell dims) as
                            return (Front.renderFrame cells winSize dims)

stepIteration :: Double -> ViewPort -> Float -> SIRSSimHandle -> IO SIRSSimHandle
stepIteration fixedDt viewport dtRendering hdl = PA.advanceSimulation hdl fixedDt

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