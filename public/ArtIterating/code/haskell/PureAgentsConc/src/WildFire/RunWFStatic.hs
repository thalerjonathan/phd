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

import qualified PureAgentsConc as PA

winTitle = "WildFire Static CONC"
winSize = (800, 800)

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
                        hdl <- PA.initStepSimulation ignitedAs ()
                        stepWithRendering hdl dt cells

runWFStaticSteps :: IO ()
runWFStaticSteps = do
                    let dt = 1.0
                    let xCells = 50
                    let yCells = 50
                    let rngSeed = 42
                    let cells = (xCells, yCells)
                    let g = mkStdGen rngSeed
                    -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
                    (as, g') <- atomically $ createRandomWFAgents g cells
                    let ignitedAs = initialIgnition as (25, 25) cells
                    let stepCount = 1000
                    as' <- PA.stepSimulation ignitedAs () dt stepCount
                    mapM (putStrLn . show . PA.state) as'
                    return ()

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
stepWithRendering hdl dt cells = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                5
                                hdl
                                (modelToPicture cells)
                                (stepIteration dt)

modelToPicture :: (Int, Int) -> WFSimHandle -> IO GLO.Picture
modelToPicture cells hdl = return (Front.renderFrame observableAgentStates winSize cells)
    where
        as = PA.extractHdlAgents hdl
        observableAgentStates = map (wfAgentToObservableState cells) as

stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = PA.advanceSimulation hdl fixedDt

wfAgentToObservableState :: (Int, Int) -> WFAgent -> Front.RenderCell
wfAgentToObservableState (xCells, yCells) a = Front.RenderCell { Front.renderCellCoord = (x, y),
                                                                    Front.renderCellColor = cs }
    where
        aid = PA.agentId a
        s = PA.state a
        y = floor((fromIntegral aid) / (fromIntegral xCells))
        x = mod aid yCells
        shade = burnable s
        cs = case (wfState s) of
                            Living -> (0.0, shade, 0.0)
                            Burning -> (shade, 0.0, 0.0)
                            Dead -> (0.5, 0.5, 0.5)