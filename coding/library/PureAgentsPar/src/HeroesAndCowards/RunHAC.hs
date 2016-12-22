module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import qualified PureAgentsPar as PA

import System.Random

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
stepHAC :: IO ()
stepHAC = do
        let dt = 0.025
        let agentCount = 500
        let heroDistribution = 0.5
        let rngSeed = 42
        let steps = 1
        let g = mkStdGen rngSeed
        let e = (Just 42) :: (Maybe HACEnvironment)
        let (as, g') = createRandomHACAgents g agentCount heroDistribution
        let (as', e') = PA.stepSimulation as e dt steps
        outs <- mapM (putStrLn . show . PA.state) as'
        putStrLn (show e')
        return ()

runHAC :: IO ()
runHAC = do
        let dt = 0.025
        let agentCount = 500
        let heroDistribution = 0.5
        let rngSeed = 42
        let g = mkStdGen rngSeed
        let e = (Just 42) :: (Maybe HACEnvironment)
        let (as, g') = createRandomHACAgents g agentCount heroDistribution
        --let as = createHACTestAgents
        let (as', hdl) = PA.initStepSimulation as e
        stepWithRendering hdl dt


stepWithRendering :: HACSimHandle -> Double -> IO ()
stepWithRendering hdl dt = GLO.simulateIO Front.display
                                GLO.white
                                25
                                hdl
                                modelToPicture
                                (stepIteration dt)

-- A function to convert the model to a picture.
modelToPicture :: HACSimHandle -> IO GLO.Picture
modelToPicture hdl = return (Front.renderFrame observableAgentStates)
    where
        as = PA.extractAgents hdl
        observableAgentStates = map hacAgentToObservableState as

-- A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
-- NOTE: atomically is VERY important, if it is not there there then the STM-transactions would not occur!
--       NOTE: this is actually wrong, we can avoid atomically as long as we are running always on the same thread.
--             atomically would commit the changes and make them visible to other threads
stepIteration :: Double -> GLO.ViewPort -> Float -> HACSimHandle -> IO HACSimHandle
stepIteration fixedDt viewport dtRendering hdl = do
                                                    let (as', e', hdl') = PA.advanceSimulation hdl fixedDt
                                                    return hdl'

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = PA.state a
        (x, y) = pos s
        h = hero s
--------------------------------------------------------------------------------------------------------------------------------------------------
