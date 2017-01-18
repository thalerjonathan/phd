module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import Control.Concurrent.STM.TVar

import qualified PureAgentsAct as PA

import System.Random

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
runHAC :: IO ()
runHAC = do
        let dt = 0.05
        let agentCount = 3
        let heroDistribution = 0.25
        let rngSeed = 42
        let g = mkStdGen rngSeed
        e <- PA.atomically $ newTVar 42
        (as, g') <- PA.atomically $ createRandomHACAgents g agentCount heroDistribution
        --let as = createHACTestAgents
        hdl <- PA.startSimulation as dt e
        stepWithRendering hdl


stepWithRendering :: HACSimHandle -> IO ()
stepWithRendering hdl = GLO.simulateIO Front.display
                            GLO.white
                            1
                            hdl
                            modelToPicture
                            stepIteration

-- A function to convert the model to a picture.
modelToPicture :: HACSimHandle -> IO GLO.Picture
modelToPicture hdl = do
                        as <- PA.observeAgentStates hdl
                        let observableAgentStates = map hacAgentToObservableState as
                        return (Front.renderFrame observableAgentStates)

stepIteration :: GLO.ViewPort -> Float -> HACSimHandle -> IO HACSimHandle
stepIteration viewport dtRendering hdl = return hdl

hacAgentToObservableState :: HACAgentState -> (Double, Double, Bool)
hacAgentToObservableState s = (x, y, h)
    where
        (x, y) = pos s
        h = hero s
--------------------------------------------------------------------------------------------------------------------------------------------------
