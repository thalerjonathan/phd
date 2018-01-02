module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import Control.Concurrent.STM.TVar

import qualified PureAgentsConc as PA

import System.Random
import Control.Monad.STM

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO

runHAC :: IO ()
runHAC = do
        let dt = 0.05
        let agentCount = 300
        let heroDistribution = 0.25
        let rngSeed = 42
        let g = mkStdGen rngSeed
        e <- atomically $ newTVar 42
        (as, g') <- atomically $ createRandomHACAgents g agentCount heroDistribution
        hdl <- PA.initStepSimulation as e
        stepWithRendering hdl dt

stepHAC :: IO ()
stepHAC = do
        let dt = 0.025
        let agentCount = 100
        let heroDistribution = 0.5
        let rngSeed = 42
        let steps = 1000
        let g = mkStdGen rngSeed
        e <- atomically $ newTVar 42
        (as, g') <- atomically $ createRandomHACAgents g agentCount heroDistribution
        as' <- PA.stepSimulation as e dt steps
        outs <- mapM (putStrLn . show . PA.state) as'
        e' <- atomically $ readTVar e
        putStrLn (show e')
        return ()

stepWithRendering :: HACSimHandle -> Double -> IO ()
stepWithRendering hdl dt = GLO.simulateIO Front.display
                                GLO.white
                                1
                                hdl
                                modelToPicture
                                (stepIteration dt)

modelToPicture :: HACSimHandle -> IO GLO.Picture
modelToPicture hdl = return (Front.renderFrame observableAgentStates)
    where
        as = PA.extractHdlAgents hdl
        observableAgentStates = map hacAgentToObservableState as


stepIteration :: Double -> GLO.ViewPort -> Float -> HACSimHandle -> IO HACSimHandle
stepIteration fixedDt viewport dtRendering hdl = (PA.advanceSimulation hdl fixedDt)

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = PA.state a
        (x, y) = pos s
        h = hero s
