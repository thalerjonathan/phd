module Main where

import HaskellAgents
import HACModel
import Control.Monad.STM
import System.Random

import qualified HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

-- TODO: implement wildfire (read/write environment to create new agents)
-- TODO: implement SIRS model
-- TODO: implement schelling segregation

main :: IO ()
main = do
        let dt = 0.025
        let agentCount = 500
        let heroDistribution = 0.5
        let simStepsPerSecond = 30
        let rngSeed = 42
        let steps = 1000
        let g = mkStdGen rngSeed
        -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
        (initAs, g') <- atomically $ createRandomHACAgents g agentCount heroDistribution
        as <- calculateSteps steps initAs dt
        outs <- mapM (putStrLn . show . state) as
        return ()

calculateSteps :: Int -> [HACAgent] -> Double -> IO [HACAgent]
calculateSteps n as dt = do
                            as' <- atomically $ HaskellAgents.stepSimulation as dt
                            if ( n == 0 ) then
                                return as'
                                    else
                                        calculateSteps (n-1) as' dt

stepWithRendering :: [HACAgent] -> Double -> IO ()
stepWithRendering initAs dt = GLO.simulateIO Front.display
                                GLO.white
                                30
                                initAs
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: [HACAgent] -> IO GLO.Picture
modelToPicture as = return (Front.renderFrame observableAgentStates)
    where
        observableAgentStates = map hacAgentToObservableState as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> GLO.ViewPort -> Float -> [HACAgent] -> IO [HACAgent]
stepIteration fixedDt viewport dtRendering as = atomically $ HaskellAgents.stepSimulation as fixedDt

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = state a
        (x, y) = pos s
        h = hero s

