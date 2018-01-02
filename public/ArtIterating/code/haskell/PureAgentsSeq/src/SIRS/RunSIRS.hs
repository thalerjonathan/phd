module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsSeq as PA

import System.Random

winTitle = "SIRS SEQ"
winSize = (800, 800)

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
            let hdl = PA.initStepSimulation as ()
            stepWithRendering dims hdl dt

stepWithRendering :: (Int, Int) -> SIRSSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display winTitle winSize )
                                GLO.white
                                20
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

modelToPicture :: (Int, Int) -> SIRSSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            let as = PA.extractHdlAgents hdl
                            let cells = map (sirsAgentToRenderCell dims) as
                            return (Front.renderFrame cells winSize dims)


stepIteration :: Double -> ViewPort -> Float -> SIRSSimHandle -> IO SIRSSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)

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