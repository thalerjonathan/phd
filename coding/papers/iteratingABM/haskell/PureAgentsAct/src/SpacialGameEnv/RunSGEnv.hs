module SpacialGameEnv.RunSGEnv where

import SpacialGameEnv.SGModelEnv
import qualified PureAgents2DDiscrete as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsAct as PA

import System.Random
import Data.Maybe
import Data.List
import Control.Monad.STM

winTitle = "Spacial Game Env ACT"
winSize = (800, 800)

runSGEnvWithRendering :: IO ()
runSGEnvWithRendering = do
                            --hSetBuffering stdin NoBuffering
                            let dt = 1.0
                            let dims = (50, 50)
                            let rngSeed = 42
                            let defectorsRatio = 0.0
                            let g = mkStdGen rngSeed
                            (as, g') <- atomically $ createRandomSGAgents g dims defectorsRatio
                            let asWithDefector = setDefector as (25, 25) dims
                            env <- atomically $ sgEnvironmentFromAgents asWithDefector
                            hdl <- PA.startSimulation asWithDefector dt env
                            stepWithRendering dims hdl dt

setDefector :: [SGAgent] -> (Int, Int) -> (Int, Int) -> [SGAgent]
setDefector as pos cells
    | isNothing mayAgentAtPos = as
    | otherwise = infront ++ [defectedAgentAtPos] ++ (tail behind)
    where
        mayAgentAtPos = find (\a -> pos == (agentToCell a cells)) as
        agentAtPos = (fromJust mayAgentAtPos)
        agentAtPosId = PA.agentId agentAtPos
        defectedAgentAtPos = PA.updateState agentAtPos (\s -> s { sgCurrState = Defector,
                                                                   sgPrevState = Defector } )
        (infront, behind) = splitAt agentAtPosId as

stepWithRendering :: (Int, Int) -> SGSimHandle -> Double -> IO ()
stepWithRendering dims hdl dt = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                30
                                hdl
                                (modelToPicture dims)
                                (stepIteration dt)

modelToPicture :: (Int, Int) -> SGSimHandle -> IO GLO.Picture
modelToPicture dims hdl = do
                            as <- PA.observeAgentStates hdl
                            let cells = map (sgAgentToRenderCell dims) as
                            return (Front.renderFrame cells winSize dims)

stepIteration :: Double -> ViewPort -> Float -> SGSimHandle -> IO SGSimHandle
stepIteration fixedDt viewport dtRendering hdl = return hdl

sgAgentToRenderCell :: (Int, Int) -> (PA.AgentId, Double, SGAgentState) -> Front.RenderCell
sgAgentToRenderCell (xDim, yDim) (aid, _, s) = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                                    Front.renderCellColor = ss }
    where
        ax = mod aid yDim
        ay = floor((fromIntegral aid) / (fromIntegral xDim))
        curr = sgCurrState s
        prev = sgPrevState s
        ss = sgAgentStateToColor prev curr

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