module SIRS.RunSIRS where

import SIRS.SIRSModel

import qualified GridRenderer as Front
import qualified Graphics.Gloss as GLO
import qualified MinABS as ABS

import System.Random
import Graphics.Gloss.Interface.IO.Simulate

winSize = (800, 800)
winTitle = "SIRS MinABS"

rngSeed = 42
cells = (70, 70)
initInfectionProb = 0.2

runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
                        --hSetBuffering stdin NoBuffering
                        initRng rngSeed

                        as <- createRandomSIRSAgents cells initInfectionProb

                        let as' = ABS.iterationStart as

                        stepWithRendering as'

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender = do
                            --hSetBuffering stdin NoBuffering
                            let steps = 1

                            as <- createRandomSIRSAgents cells initInfectionProb

                            let as' = ABS.iterationSteps as steps

                            pic <- modelToPicture as'
                            GLO.display (Front.display winTitle winSize) GLO.white pic

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

stepWithRendering :: [SIRSAgent] -> IO ()
stepWithRendering as = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                as
                                modelToPicture
                                stepIteration

modelToPicture :: [SIRSAgent] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map (sirsAgentToRenderCell cells) as
                        return (Front.renderFrame rcs winSize cells)

stepIteration :: ViewPort -> Float -> [SIRSAgent] -> IO [SIRSAgent]
stepIteration viewport dtRendering as = return (ABS.iterationNext as)

sirsAgentToRenderCell :: (Int, Int) -> SIRSAgent -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = ABS.s a
        (ax, ay) = (sirsCoord s)
        ss = case (sirsState s) of
                        Susceptible -> (0.0, 0.4, 0.0)
                        Infected -> (0.7, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 0.7)