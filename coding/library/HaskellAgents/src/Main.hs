module Main where

import WildfireModelDynamic

import qualified Data.HashMap as Map
import qualified WildFireFrontend as Front

import Control.Monad.STM
import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified HaskellAgents as Agent

main :: IO ()
main = do
            let dt = 1.0
            let xCells = 100
            let yCells = 100
            let rngSeed = 42
            let cells = (xCells, yCells)
            let g = mkStdGen rngSeed
            let env = createEnvironment cells
            let c = fromJust (cellByCoord env (50, 50))
            (a, g') <- atomically $ igniteCell g c
            as <- atomically $ Agent.initStepSimulation [a] (Just env)
            stepWithRendering as dt

stepWithRendering :: [WFAgent] -> Double -> IO ()
stepWithRendering as dt = simulateIO Front.display
                                GLO.white
                                10
                                as
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: [WFAgent] -> IO GLO.Picture
modelToPicture as = do
                        env <- atomically $ Agent.readEnv (head as)     -- NOTE: dirty hack, but gloss is very restrictive!
                        let cs = cells env
                        let limits = cellLimits env
                        return (Front.renderFrame (Map.elems cs) limits)


-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> [WFAgent] -> IO [WFAgent]
stepIteration fixedDt viewport dtRendering as = do
                                                    (as', e') <- atomically $ Agent.advanceSimulation as fixedDt
                                                    return as'
--------------------------------------------------------------------------------------------------------------------------------------------------
