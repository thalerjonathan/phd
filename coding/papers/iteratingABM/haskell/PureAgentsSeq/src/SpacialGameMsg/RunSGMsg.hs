module SpacialGameMsg.RunSGMsg where

import SpacialGameMsg.SGModelMsg
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsSeq as PA

import System.Random
import Data.Maybe
import Data.List

runSGMsgWithRendering :: IO ()
runSGMsgWithRendering = do
                            --hSetBuffering stdin NoBuffering
                            let dt = 1.0
                            let dims = (50, 50)
                            let rngSeed = 42
                            let defectorsRatio = 0.0
                            let g = mkStdGen rngSeed
                            let (as, g') = createRandomSGAgents g dims defectorsRatio
                            let asWithDefector = setDefector as (25, 25) dims
                            let hdl = PA.initStepSimulation asWithDefector ()
                            stepWithRendering dims hdl dt

runSGMsgStepsAndRender :: IO ()
runSGMsgStepsAndRender = do
                            --hSetBuffering stdin NoBuffering
                            let dt = 1.0
                            let dims = (50, 50)
                            let winSize = (800, 800)
                            let steps = 45
                            let rngSeed = 42
                            let defectorsRatio = 0.0
                            let g = mkStdGen rngSeed

                            let (as, g') = createRandomSGAgents g dims defectorsRatio
                            let asWithDefector = setDefector as (25, 25) dims

                            let (as', _) = PA.stepSimulation asWithDefector () dt steps

                            let observableAgentStates =  map (sgAgentToRenderCell dims) as'
                            let frameRender = (Front.renderFrame observableAgentStates winSize dims)
                            GLO.display (Front.display "Spacial Game MSG Seq" winSize) GLO.white frameRender
                            return ()



setDefector :: [SGAgent] -> (Int, Int) -> (Int, Int) -> [SGAgent]
setDefector as pos cells
    | isNothing mayAgentAtPos = as
    | otherwise = infront ++ [defectedAgentAtPos] ++ (tail behind)
    where
        mayAgentAtPos = find (\a -> pos == (agentToCell a cells)) as
        agentAtPos = (fromJust mayAgentAtPos)
        agentAtPosId = PA.agentId agentAtPos
        defectedAgentAtPos = PA.updateState agentAtPos (\s -> s { sgCurrState = Defector, sgPrevState = Defector } )
        (infront, behind) = splitAt agentAtPosId as

stepWithRendering :: (Int, Int) -> SGSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display "Spacial Game Seq" (800, 800))
                                GLO.white
                                1
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> SGSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            let as = PA.extractHdlAgents hdl
                            let cells = map (sgAgentToRenderCell dims) as
                            return (Front.renderFrame cells (800, 800) dims)

sgAgentToRenderCell :: (Int, Int) -> SGAgent -> Front.RenderCell
sgAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        id = PA.agentId a
        s = PA.state a
        ax = mod id yDim
        ay = floor((fromIntegral id) / (fromIntegral xDim))
        curr = sgCurrState s
        prev = sgPrevState s
        ss = sgAgentStateToColor prev curr

-- NOTE: read it the following way: "the agent was in state X following another one Y" => first parameter is prev, second is curr
sgAgentStateToColor :: SGState -> SGState -> (Double, Double, Double)
sgAgentStateToColor Cooperator Cooperator = blueC
sgAgentStateToColor Defector Defector = redC
sgAgentStateToColor Defector Cooperator = yellowC
sgAgentStateToColor Cooperator Defector = greenC

blueC :: (Double, Double, Double)
blueC = (0.0, 0.0, 1.0)

greenC :: (Double, Double, Double)
greenC = (0.0, 1.0, 0.0)

redC :: (Double, Double, Double)
redC = (1.0, 0.0, 0.0)

yellowC :: (Double, Double, Double)
yellowC = (1.0, 1.0, 0.0)

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> SGSimHandle -> IO SGSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)
--------------------------------------------------------------------------------------------------------------------------------------------------