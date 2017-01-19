module WildFire.RunWFDynamic where

import WildFire.WildFireModelDynamic

import qualified Data.Map as Map
import qualified PureAgents2DDiscrete as Front

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsAct as PA

runWFDynamicRendering :: IO ()
runWFDynamicRendering = do
                            let dt = 1.0
                            let xCells = 100
                            let yCells = 100
                            let rngSeed = 42
                            let cells = (xCells, yCells)
                            let g = mkStdGen rngSeed
                            env <- PA.atomically $ createEnvironment cells
                            let c = fromJust (cellByCoord env (50, 50))
                            (a, g') <- PA.atomically $ igniteCell g c
                            hdl <- PA.startSimulation [a] dt env
                            stepWithRendering hdl dt

stepWithRendering :: WFSimHandle -> Double -> IO ()
stepWithRendering hdl dt = simulateIO (Front.display "WildFire Dynamic ACT" (800, 800))
                                GLO.white
                                30
                                hdl
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: WFSimHandle -> IO GLO.Picture
modelToPicture hdl = do
                        let env = PA.extractHdlEnv hdl
                        let cs = cells env
                        let limits = cellLimits env
                        renderCells <- atomically $ mapM wfCellToRenderCell (Map.elems cs)
                        return (Front.renderFrame renderCells (800, 800) limits)

wfCellToRenderCell :: TVar WFCell -> STM Front.RenderCell
wfCellToRenderCell cVar = do
                            c <- readTVar cVar
                            let shade = burnable c
                            let cs = case (cellState c) of
                                        Living -> (0.0, shade, 0.0)
                                        Burning -> (shade, 0.0, 0.0)
                                        Dead -> (0.5, 0.5, 0.5)
                            return Front.RenderCell { Front.renderCellCoord = (coord c),
                                                Front.renderCellColor = cs}


-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = return hdl
--------------------------------------------------------------------------------------------------------------------------------------------------
