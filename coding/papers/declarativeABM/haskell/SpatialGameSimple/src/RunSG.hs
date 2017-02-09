module RunSG where

import SGModel
import qualified SGRenderer as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import System.Random
import Data.Maybe
import Data.List
import System.IO
import qualified Data.Map as Map

winSize = (1000, 1000)
winTitle = "Spatial Game Simple"

runSGWithRendering :: IO ()
runSGWithRendering = do
                            let dims = (99, 99)
                            let cs = createSGCells dims
                            let csWithDefector = setDefector cs (49, 49) dims
                            stepWithRendering dims csWithDefector

runSGStepsAndRender :: IO ()
runSGStepsAndRender = do
                            hSetBuffering stdout NoBuffering
                            let steps = 30
                            let dims = (99, 99)
                            let cs = createSGCells dims
                            let csWithDefector = setDefector cs (49, 49) dims

                            cs' <- stepMulti csWithDefector steps
{-
                            let cells = map sgSGCellToRenderCell (Map.elems cs')
                            let frameRender = (Front.renderFrame cells winSize dims)
                            GLO.display (Front.display winTitle winSize) GLO.white frameRender
-}
                            mapM (putStrLn . show) cs'
                            return ()

stepMulti :: CellContainer -> Int -> IO CellContainer
stepMulti cs 0 = return cs
stepMulti cs n = stepMulti cs' (n-1)
    where
        cs' = stepSingle cs

stepWithRendering :: (Int, Int) -> CellContainer -> IO ()
stepWithRendering dims cs = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                1
                                cs
                                (modelToPicture dims)
                                stepIteration

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> CellContainer -> IO GLO.Picture
modelToPicture dims cs = do
                            let cells = map sgSGCellToRenderCell (Map.elems cs)
                            return (Front.renderFrame cells winSize dims)

sgSGCellToRenderCell :: SGCell -> Front.RenderCell
sgSGCellToRenderCell c = Front.RenderCell { Front.renderCellCoord = coords,
                                                        Front.renderCellColor = ss }
    where
        coords = sgCoords c
        curr = sgCurrState c
        prev = sgPrevState c
        ss = sgCellStateToColor prev curr

-- NOTE: read it the following way: "the agent was in state X following another one Y" => first parameter is prev, second is curr
sgCellStateToColor :: SGState -> SGState -> (Double, Double, Double)
sgCellStateToColor Cooperator Cooperator = blueC
sgCellStateToColor Defector Defector = redC
sgCellStateToColor Defector Cooperator = greenC
sgCellStateToColor Cooperator Defector = yellowC

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
stepIteration :: ViewPort -> Float -> CellContainer -> IO CellContainer
stepIteration viewport dtRendering cs = return (stepSingle cs)
--------------------------------------------------------------------------------------------------------------------------------------------------