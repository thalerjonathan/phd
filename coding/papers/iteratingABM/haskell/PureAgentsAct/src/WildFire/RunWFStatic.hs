module WildFire.RunWFStatic where

import WildFire.WildFireModelStatic

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import qualified Data.Map as Map
import qualified PureAgents2DDiscrete as Front

import System.Random
import System.IO
import Data.Maybe
import Data.List
import Debug.Trace

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsAct as PA

runWFStaticRendering :: IO ()
runWFStaticRendering = do
                        let dt = 1.0
                        let xCells = 50
                        let yCells = 50
                        let rngSeed = 42
                        let cells = (xCells, yCells)
                        let g = mkStdGen rngSeed
                        -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
                        (as, g') <- atomically $ createRandomWFAgents g cells
                        let ignitedAs = initialIgnition as (25, 25) cells
                        hdl <- PA.startSimulation ignitedAs dt ()
                        stepWithRendering hdl dt cells

initialIgnition :: [WFAgent] -> (Int, Int) -> (Int, Int) -> [WFAgent]
initialIgnition as pos cells
    | isNothing mayAgentAtPos = as
    | otherwise = infront ++ [ignitedAgentAtPos] ++ (tail behind)
    where
        mayAgentAtPos = find (\a -> pos == (agentToCell a cells)) as
        agentAtPos = (fromJust mayAgentAtPos)
        agentAtPosId = PA.agentId agentAtPos
        ignitedAgentAtPos = igniteAgent agentAtPos
        (infront, behind) = splitAt agentAtPosId as

stepWithRendering :: WFSimHandle -> Double -> (Int, Int) -> IO ()
stepWithRendering hdl dt cells = simulateIO (Front.display "WildFire Static ACT" (800, 800))
                                GLO.white
                                30
                                hdl
                                (modelToPicture cells)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> WFSimHandle -> IO GLO.Picture
modelToPicture cells hdl = do
                            as <- PA.observeAgentStates hdl
                            let observableAgentStates = map (wfAgentToObservableState cells) as
                            return (Front.renderFrame observableAgentStates (800, 800) cells)

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = return hdl

wfAgentToObservableState :: (Int, Int) -> (PA.AgentId, Double, WFAgentState) -> Front.RenderCell
wfAgentToObservableState (xCells, yCells) (aid, _, s) = Front.RenderCell { Front.renderCellCoord = (x, y),
                                                                    Front.renderCellColor = cs }
    where
        y = floor((fromIntegral aid) / (fromIntegral xCells))
        x = mod aid yCells
        shade = burnable s
        cs = case (wfState s) of
                            Living -> (0.0, shade, 0.0)
                            Burning -> (shade, 0.0, 0.0)
                            Dead -> (0.5, 0.5, 0.5)