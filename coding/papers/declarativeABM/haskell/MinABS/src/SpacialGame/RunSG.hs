module SpacialGame.RunSG where

import SpacialGame.SGModel
import qualified GridRenderer as Front
import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Animate

import qualified MinABS as ABS

import System.Random
import System.IO
import Data.Maybe
import Data.List
import Data.IORef

winSize = (1000, 1000)
winTitle = "Spatial Game MinABS"
rngSeed = 42

runSGWithRendering :: IO ()
runSGWithRendering = do
                            let dims = (39, 39)
                            let defectorsRatio = 0.0

                            initRng rngSeed

                            as <- createRandomSGAgents dims defectorsRatio
                            let asWithDefector = setDefector as (19, 19) dims

                            let as' = ABS.startIteration asWithDefector
                            asRef <- newIORef as'
                            stepWithRendering dims asRef

runSGStepsAndRender :: IO ()
runSGStepsAndRender = do
                            hSetBuffering stdin NoBuffering
                            let dims = (39, 39)
                            let steps = 20
                            let defectorsRatio = 0.0

                            initRng rngSeed

                            as <- createRandomSGAgents dims defectorsRatio
                            let asWithDefector = setDefector as (19, 19) dims

                            let as' = ABS.iterationSteps asWithDefector steps

                            -- mapM (putStrLn . show) as'
                            putStrLn $ show (head as')

                            --pic <- (renderFrame as' dims)
                            --GLO.display (Front.display winTitle winSize) GLO.white pic
                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

stepWithRendering :: (Int, Int) -> IORef [SGAgent] -> IO ()
stepWithRendering dims asRef = animateIO (Front.display winTitle winSize)
                                        GLO.white
                                        (nextFrame asRef dims)
                                        (\_ -> return () )

nextFrame :: IORef [SGAgent] -> (Int, Int) -> Float -> IO Picture
nextFrame asRef dims _ = do
                            as <- readIORef asRef
                            let as' = (ABS.iteration as)
                            writeIORef asRef as'
                            renderFrame as' dims

renderFrame :: [SGAgent] -> (Int, Int) -> IO Picture
renderFrame as dims = do
                        let cells = map (sgAgentToRenderCell dims) as
                        return (Front.renderFrame cells winSize dims)

sgAgentToRenderCell :: (Int, Int) -> SGAgent -> Front.RenderCell
sgAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        id = ABS.aid a
        s = ABS.s a
        ax = mod id yDim
        ay = floor((fromIntegral id) / (fromIntegral xDim))
        curr = sgCurrState s
        prev = sgPrevState s
        ss = sgAgentStateToColor prev curr

-- NOTE: read it the following way: "the agent was in state X following another one Y" => first parameter is prev, second is curr
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
--------------------------------------------------------------------------------------------------------------------------------------------------