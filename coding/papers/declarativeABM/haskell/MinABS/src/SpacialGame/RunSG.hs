module SpacialGame.RunSG where

import SpacialGame.SGModel
import qualified GridRenderer as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified MinABS as ABS

import System.Random
import System.IO
import Data.Maybe
import Data.List

winSize = (1000, 1000)
winTitle = "Spatial Game MinABS"
rngSeed = 42

runSGWithRendering :: IO ()
runSGWithRendering = do
                            let dims = (49, 49)
                            let defectorsRatio = 0.0

                            initRng rngSeed

                            as <- createRandomSGAgents dims defectorsRatio
                            let asWithDefector = setDefector as (24, 24) dims

                            let as' = ABS.startIteration asWithDefector
                            stepWithRendering dims as'

runSGStepsAndRender :: IO ()
runSGStepsAndRender = do
                            hSetBuffering stdin NoBuffering
                            let dims = (49, 49)
                            let steps = 30
                            let defectorsRatio = 0.0

                            initRng rngSeed

                            as <- createRandomSGAgents dims defectorsRatio
                            let asWithDefector = setDefector as (24, 24) dims

                            let as' = ABS.iterationSteps asWithDefector steps

                            mapM (putStrLn . show) as'
{-
                            let observableAgentStates = map (sgAgentToRenderCell dims) as'
                            let frameRender = (Front.renderFrame observableAgentStates winSize dims)
                            GLO.display (Front.display winTitle winSize) GLO.white frameRender
                            -}
                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

stepWithRendering :: (Int, Int) -> [SGAgent] -> IO ()
stepWithRendering dims as = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                as
                                (modelToPicture dims)
                                stepIteration

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> [SGAgent] -> IO GLO.Picture
modelToPicture dims as = do
                            let cells = map (sgAgentToRenderCell dims) as
                            return (Front.renderFrame cells winSize dims)

sgAgentToRenderCell :: (Int, Int) -> SGAgent -> Front.RenderCell
sgAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        id = ABS.aid a
        s = ABS.s a
        ax = mod id yDim
        ay = floor((fromIntegral id) / (fromIntegral xDim))
        curr = sgCurrState s
        prev = sgPrevState s
        ss = sgAgentStateToColor prev curr

-- NOTE: read it the following way: "the agent was in state X following another one Y" => first parameter is prev, second is curr
sgAgentStateToColor :: SGState -> SGState -> (Double, Double, Double)
sgAgentStateToColor Cooperator Cooperator = blueC
sgAgentStateToColor Defector Defector = redC
sgAgentStateToColor Defector Cooperator = greenC
sgAgentStateToColor Cooperator Defector = yellowC

blueC :: (Double, Double, Double)
blueC = (0.0, 0.0, 0.7)

greenC :: (Double, Double, Double)
greenC = (0.0, 0.4, 0.0)

redC :: (Double, Double, Double)
redC = (0.7, 0.0, 0.0)

yellowC :: (Double, Double, Double)
yellowC = (1.0, 0.9, 0.0)

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: ViewPort -> Float -> [SGAgent] -> IO [SGAgent]
stepIteration viewport dtRendering as = return (ABS.iteration as)
--------------------------------------------------------------------------------------------------------------------------------------------------