module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import qualified PureAgentsPar as PA

import System.Random

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO
import qualified Graphics.Gloss as GLO

--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
runHAC :: IO ()
runHAC = do
        let dt = 0.04
        let agentCount = 75
        let heroDistribution = 0.5
        let rngSeed = 123
        let g = mkStdGen rngSeed
        let (as, g') = createRandomHACAgents g agentCount heroDistribution
        let env = hacEnvironmentFromAgents as
        let hdl = PA.initStepSimulation as env
        stepWithRendering hdl dt

stepHACWithRendering :: IO ()
stepHACWithRendering = do
                        let dt = 0.025
                        let agentCount = 1000
                        let heroDistribution = 0.25
                        let rngSeed = 42
                        let steps = 10
                        let g = mkStdGen rngSeed
                        let (as, g') = createRandomHACAgents g agentCount heroDistribution
                        let env = hacEnvironmentFromAgents as
                        let (as', e') = PA.stepSimulation as env dt steps
                        let observableAgentStates = map hacAgentToObservableState as'
                        GLO.display (Front.display) GLO.white (Front.renderFrame observableAgentStates)
                        return ()

stepHAC :: IO ()
stepHAC = do
            let dt = 0.025
            let agentCount = 1000
            let heroDistribution = 0.25
            let rngSeed = 42
            let steps = 500
            let g = mkStdGen rngSeed
            let (as, g') = createRandomHACAgents g agentCount heroDistribution
            let env = hacEnvironmentFromAgents as
            let (as', e') = PA.stepSimulation as env dt steps
            let observableAgentStates = map hacAgentToObservableState as'
            mapM (putStrLn . show) observableAgentStates
            return ()

stepWithRendering :: HACSimHandle -> Double -> IO ()
stepWithRendering hdl dt = GLO.simulateIO Front.display
                                GLO.white
                                30
                                hdl
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: HACSimHandle -> IO GLO.Picture
modelToPicture hdl = return (Front.renderFrame observableAgentStates)
    where
        as = PA.extractHdlAgents hdl
        observableAgentStates = map hacAgentToObservableState as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> GLO.ViewPort -> Float -> HACSimHandle -> IO HACSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = PA.state a
        (x, y) = pos s
        h = hero s
--------------------------------------------------------------------------------------------------------------------------------------------------
