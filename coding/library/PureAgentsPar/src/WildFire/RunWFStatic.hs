module WildFire.RunWFStatic where

import WildFire.WildFireModelStatic

import qualified Data.Map as Map
import qualified PureAgents2DDiscrete as Front

import System.Random
import System.IO
import Data.Maybe
import Data.List
import Debug.Trace

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsPar as PA

runWFStaticRendering :: IO ()
runWFStaticRendering = do
                        let dt = 1.0
                        let xCells = 100
                        let yCells = 100
                        let rngSeed = 42
                        let cells = (xCells, yCells)
                        let g = mkStdGen rngSeed
                        -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
                        let (as, g') = createRandomWFAgents g cells
                        let ignitedAs = initialIgnition as (50, 50) cells
                        let hdl = PA.initStepSimulation ignitedAs ()
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
                    let (as, g') = createRandomWFAgents g cells
                    let ignitedAs = initialIgnition as (25, 25) cells
                    let stepCount = 1000
                    let (as', _) = PA.stepSimulation ignitedAs () dt stepCount
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
        ignitedAgentAtPos = agentAtPos{ PA.inBox = [(agentAtPosId, Ignite)]}
        (infront, behind) = splitAt agentAtPosId as

stepWithRendering :: WFSimHandle -> Double -> (Int, Int) -> IO ()
stepWithRendering hdl dt cells = simulateIO (Front.display "WildFire Static" (800, 800))
                                GLO.white
                                5
                                hdl
                                (modelToPicture cells)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> WFSimHandle -> IO GLO.Picture
modelToPicture cells hdl = return (Front.renderFrame observableAgentStates (800, 800) cells)
    where
        as = PA.extractHdlAgents hdl
        observableAgentStates = map (wfAgentToObservableState cells) as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)

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