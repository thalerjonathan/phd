module Main where

import WildfireModel

import qualified WildFireFrontend as Front

import Control.Monad.STM
import System.Random
import System.IO

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified HaskellAgents as Agents

-- TODO: fix infinite loop of messages!

-- TODO: implement dynamic wildfire (read/write environment to create new agents)
-- TODO: implement schelling segregation

main :: IO ()
main = do
           let dt = 1.0
           let xCells = 70
           let yCells = 70
           let rngSeed = 42
           let cells = (xCells, yCells)
           let g = mkStdGen rngSeed
           -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
           (as, g') <- atomically $ createRandomWFAgents g cells
           as' <- atomically $ Agents.initStepSimulation as Nothing
           atomically $ Agents.sendMsg (head as') Ignite 1
           stepWithRendering as' dt cells

stepWithRendering :: [WFAgent] -> Double -> (Int, Int) -> IO ()
stepWithRendering as dt cells = simulateIO Front.display
                                GLO.white
                                10
                                as
                                (modelToPicture cells)
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: (Int, Int) -> [WFAgent] -> IO GLO.Picture
modelToPicture cells as = return (Front.renderFrame observableAgentStates cells)
    where
        observableAgentStates = map (wfAgentToObservableState cells) as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> ViewPort -> Float -> [WFAgent] -> IO [WFAgent]
stepIteration fixedDt viewport dtRendering as = do
                                                    (as', e') <- atomically $ Agents.advanceSimulation as fixedDt
                                                    return as'

wfAgentToObservableState :: (Int, Int) -> WFAgent -> (Int, Int, WFState, Double)
wfAgentToObservableState (xCells, yCells) a = (x, y, (wfState s), (burnable s))
    where
        aid = Agents.agentId a
        s = Agents.state a
        y = floor((fromIntegral aid) / (fromIntegral xCells))
        x = mod aid yCells
--------------------------------------------------------------------------------------------------------------------------------------------------
