module WildFire.RunWFStatic where

import WildFire.WildFireModelStatic

import qualified Data.HashMap as Map
import qualified WildFire.WildFireFrontend as Front

import Control.Monad.STM
import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsSTM as PA

runWFStatic :: IO ()
runWFStatic = do
               let dt = 1.0
               let xCells = 20
               let yCells = 20
               let rngSeed = 42
               let cells = (xCells, yCells)
               let g = mkStdGen rngSeed
               -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
               (as, g') <- atomically $ createRandomWFAgents g cells
               (as', hdl) <- atomically $ PA.initStepSimulation as Nothing
               atomically $ PA.sendMsg (head as') Ignite 1
               stepWithRendering hdl dt cells

stepWithRendering :: WFSimHandle -> Double -> (Int, Int) -> IO ()
stepWithRendering hdl dt cells = simulateIO Front.display
                                GLO.white
                                10
                                hdl
                                (modelToPicture cells)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> WFSimHandle -> IO GLO.Picture
modelToPicture cells hdl = return (Front.renderFrame observableAgentStates cells)
    where
        as = PA.extractAgents hdl
        observableAgentStates = map (wfAgentToObservableState cells) as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = do
                                                    (as', e', hdl') <- atomically $ PA.advanceSimulation hdl fixedDt
                                                    return hdl'

wfAgentToObservableState :: (Int, Int) -> WFAgent -> Front.RenderCell
wfAgentToObservableState (xCells, yCells) a = Front.RenderCell { Front.renderCellCoord = (x, y),
                                                                    Front.renderCellShade = (burnable s),
                                                                    Front.renderCellState = cellState }
    where
        aid = PA.agentId a
        s = PA.state a
        y = floor((fromIntegral aid) / (fromIntegral xCells))
        x = mod aid yCells
        cellState = case (wfState s) of
                        Living -> Front.ShadeGreen
                        Burning -> Front.ShadeRed
                        Dead -> Front.ShadeGray
