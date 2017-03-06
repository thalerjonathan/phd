module SIRS.RunSIRS where

import SIRS.SIRSModel
import qualified Agent.Agents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified Agent.Agent as FrABS

import System.Random

winSize = (800, 800)
winTitle = "SIRS FrABS"

rngSeed = 42
cells = (70, 70)
initInfectionProb = 0.2

{-
runSIRSWithRendering :: IO ()
runSIRSWithRendering = do
                        --hSetBuffering stdin NoBuffering
                        initRng rngSeed

                        as <- createRandomSIRSAgents cells initInfectionProb

                        let as' = FrABS.processIO as

                        stepWithRendering as'
-}

runSIRSStepsAndRender :: IO ()
runSIRSStepsAndRender = do
                            --hSetBuffering stdin NoBuffering
                            let steps = 1

                            as <- createRandomSIRSAgents cells initInfectionProb

                            let ass = FrABS.processSteps as 1.0 steps
                            let as' = last ass

                            pic <- modelToPicture as'
                            GLO.display (Front.display winTitle winSize) GLO.white pic

                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

{-
stepWithRendering :: [SIRSAgentDef] -> IO ()
stepWithRendering as = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                as
                                modelToPicture
                                stepIteration
-}

modelToPicture :: [SIRSAgentOut] -> IO GLO.Picture
modelToPicture as = do
                        let rcs = map (sirsAgentToRenderCell cells) as
                        return (Front.renderFrame rcs winSize cells)

{-
stepIteration :: ViewPort -> Float -> [SIRSAgentOut] -> IO [SIRSAgent]
stepIteration viewport dtRendering as = return (ABS.iterationNext as)
-}

sirsAgentToRenderCell :: (Int, Int) -> SIRSAgentOut -> Front.RenderCell
sirsAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = FrABS.aoState a
        (ax, ay) = (sirsCoord s)
        ss = case (sirsState s) of
                        Susceptible -> (0.0, 0.55, 0.0)
                        Infected -> (0.7, 0.0, 0.0)
                        Recovered -> (0.0, 0.0, 0.7)