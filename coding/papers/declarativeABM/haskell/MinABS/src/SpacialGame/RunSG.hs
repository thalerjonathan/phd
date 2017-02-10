module SpacialGame.RunSG where

import SpacialGame.SGModel

import qualified GridRenderer as Front
import qualified Graphics.Gloss as GLO
import qualified MinABS as ABS

import System.Random
import System.IO
import Data.IORef
import Graphics.Gloss.Interface.IO.Animate

winSize = (1000, 1000)
winTitle = "Spatial Game MinABS"

rngSeed = 42

cells = (99, 99)
defectorCoord = (49, 49)
defectorsRatio = 0.0


runSGWithRendering :: IO ()
runSGWithRendering = do
                            initRng rngSeed

                            as <- createRandomSGAgents cells defectorsRatio
                            let asWithDefector = setDefector as defectorCoord cells

                            let as' = ABS.iterationStart asWithDefector
                            asRef <- newIORef as'
                            stepWithRendering asRef

runSGStepsAndRender :: IO ()
runSGStepsAndRender = do
                            hSetBuffering stdin NoBuffering

                            let steps = 62

                            initRng rngSeed

                            as <- createRandomSGAgents cells defectorsRatio
                            let asWithDefector = setDefector as defectorCoord cells

                            let as' = ABS.iterationSteps asWithDefector steps

                           -- mapM (putStrLn . show) as'
                           -- putStrLn $ show (head as')

                            pic <- renderFrame as'
                            GLO.display (Front.display winTitle winSize) GLO.white pic
                            return ()

initRng :: Int -> IO StdGen
initRng seed = do
                let g = mkStdGen seed
                setStdGen g
                return g

stepWithRendering :: IORef [SGAgent] -> IO ()
stepWithRendering asRef = animateIO (Front.display winTitle winSize)
                                        GLO.white
                                        (nextFrame asRef)
                                        (\_ -> return () )

nextFrame :: IORef [SGAgent] -> Float -> IO Picture
nextFrame asRef _ = do
                        as <- readIORef asRef
                        let as' = (ABS.iterationNext as)
                        writeIORef asRef as'
                        renderFrame as'

renderFrame :: [SGAgent] -> IO Picture
renderFrame as = do
                    let rcs = map (sgAgentToRenderCell cells) as
                    return (Front.renderFrame rcs winSize cells)

sgAgentToRenderCell :: (Int, Int) -> SGAgent -> Front.RenderCell
sgAgentToRenderCell (xDim, yDim) a = Front.RenderCell { Front.renderCellCoord = (ax, ay),
                                                        Front.renderCellColor = ss }
    where
        s = ABS.s a
        (ax, ay) = sgCoord s
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