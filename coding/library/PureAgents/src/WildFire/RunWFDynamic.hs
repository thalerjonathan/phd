module WildFire.RunWFDynamic where

import WildFire.WildFireModelDynamic

import qualified Data.HashMap as Map
import qualified WildFire.WildFireFrontend as Front

import Control.Monad.STM
import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified HaskellAgents as Agent

runWFDynamic :: IO ()
runWFDynamic = do
                let dt = 1.0
                let xCells = 100
                let yCells = 100
                let rngSeed = 42
                let cells = (xCells, yCells)
                let g = mkStdGen rngSeed
                let env = createEnvironment cells
                let c = fromJust (cellByCoord env (50, 50))
                (a, g') <- atomically $ igniteCell g c
                (as, hdl) <- atomically $ Agent.initStepSimulation [a] (Just env)
                stepWithRendering hdl dt

stepWithRendering :: WFSimHandle -> Double -> IO ()
stepWithRendering hdl dt = simulateIO Front.display
                                GLO.white
                                20
                                hdl
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: WFSimHandle -> IO GLO.Picture
modelToPicture hdl = do
                        mayEnv <- atomically $ Agent.extractEnv hdl
                        let env = (fromJust mayEnv)
                        let cs = cells env
                        let limits = cellLimits env
                        return (Front.renderFrame (map wfCellToRenderCell (Map.elems cs)) limits)

wfCellToRenderCell :: WFCell -> Front.RenderCell
wfCellToRenderCell c = Front.RenderCell { Front.renderCellCoord = (coord c),
                                                Front.renderCellShade = (burnable c),
                                                Front.renderCellState = cs }
    where
        cs = case (cellState c) of
                        Living -> Front.ShadeGreen
                        Burning -> Front.ShadeRed
                        Dead -> Front.ShadeGray


-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = do
                                                    (as, e, hdl') <- atomically $ Agent.advanceSimulation hdl fixedDt
                                                    return hdl'
--------------------------------------------------------------------------------------------------------------------------------------------------
