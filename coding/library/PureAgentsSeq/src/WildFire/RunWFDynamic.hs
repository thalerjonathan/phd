module WildFire.RunWFDynamic where

import WildFire.WildFireModelDynamic

import qualified Data.Map as Map
import qualified PureAgents2DDiscrete as Front

import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsSeq as PA

runWFDynamic :: IO ()
runWFDynamic = do
                let dt = 1.0
                let xCells = 200
                let yCells = 200
                let rngSeed = 42
                let cells = (xCells, yCells)
                let g = mkStdGen rngSeed
                let env = createEnvironment cells
                let c = fromJust (cellByCoord env (100, 100))
                let (a, env', g') = igniteCell g c env
                let (as, hdl) = PA.initStepSimulation [a] env'
                stepWithRendering hdl dt

stepWithRendering :: WFSimHandle -> Double -> IO ()
stepWithRendering hdl dt = simulateIO (Front.display "WildFire Dynamic" (800, 800))
                                GLO.white
                                10
                                hdl
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: WFSimHandle -> IO GLO.Picture
modelToPicture hdl = do
                        let env = PA.extractHdlEnv hdl
                        let cs = cells env
                        let limits = cellLimits env
                        return (Front.renderFrame (map wfCellToRenderCell (Map.elems cs)) (800, 800) limits)

wfCellToRenderCell :: WFCell -> Front.RenderCell
wfCellToRenderCell c = Front.RenderCell { Front.renderCellCoord = (coord c),
                                                Front.renderCellColor = cs}
    where
        shade = burnable c
        cs = case (cellState c) of
                    Living -> (0.0, shade, 0.0)
                    Burning -> (shade, 0.0, 0.0)
                    Dead -> (0.5, 0.5, 0.5)


-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = do
                                                    let (as, e, hdl') = PA.advanceSimulation hdl fixedDt
                                                    return hdl'
--------------------------------------------------------------------------------------------------------------------------------------------------
