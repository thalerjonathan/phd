module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import Control.Concurrent.STM.TVar

import qualified PureAgentsAct as PA

import System.Random
import System.IO
import Control.Monad.STM

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

runHAC :: IO ()
runHAC = do
            hSetBuffering stdout NoBuffering
            let dt = 0.01
            let agentCount = 1000
            let heroDistribution = 0.25
            let rngSeed = 42
            let g = mkStdGen rngSeed
            (as, g') <- atomically $ createRandomHACAgents g agentCount heroDistribution
            hdl <- PA.startSimulation as dt ()
            stepWithRendering hdl


stepWithRendering :: HACSimHandle -> IO ()
stepWithRendering hdl = GLO.simulateIO Front.display
                            GLO.white
                            30
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

hacAgentToObservableState :: (PA.AgentId, Double, HACAgentState) -> (Double, Double, Bool)
hacAgentToObservableState (_, t, s) = (x, y, h)
    where
        (x, y) = pos s
        h = hero s