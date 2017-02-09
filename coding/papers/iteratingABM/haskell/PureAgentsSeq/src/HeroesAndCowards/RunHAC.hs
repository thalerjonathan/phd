module HeroesAndCowards.RunHAC where

import HeroesAndCowards.HACModel

import qualified PureAgentsSeq as PA

import System.Random

import qualified HeroesAndCowards.HACFrontend as Front
import qualified Graphics.Gloss.Interface.IO.Simulate as GLO
import qualified Graphics.Gloss as GLO

runHAC :: IO ()
runHAC = do
        let dt = 0.04
        let agentCount = 75
        let heroDistribution = 0.5
        let rngSeed = 11
        let g = mkStdGen rngSeed
        let e = 42
        let (as, g') = createRandomHACAgents g agentCount heroDistribution
        let hdl = PA.initStepSimulation as e
        stepWithRendering hdl dt

stepHACWithRendering :: IO ()
stepHACWithRendering = do
                        let dt = 0.02
                        let agentCount = 100
                        let heroDistribution = 0.5
                        let rngSeed = 42
                        let steps = 100
                        let g = mkStdGen rngSeed
                        let (as, g') = createRandomHACAgents g agentCount heroDistribution
                        let e = 0
                        let (as', e') = PA.stepSimulation as e dt steps
                        let observableAgentStates = map hacAgentToObservableState as'
                        GLO.display (Front.display) GLO.white (Front.renderFrame observableAgentStates)
                        return ()

stepHAC :: IO ()
stepHAC = do
        let dt = 0.1
        let agentCount = 2000
        let heroDistribution = 0.25
        let rngSeed = 42
        let steps = 1000
        let g = mkStdGen rngSeed
        let e = 42
        let (as, g') = createRandomHACAgents g agentCount heroDistribution
        let (as', e') = PA.stepSimulation as e dt steps
        outs <- mapM (putStrLn . show . PA.state) as'
        putStrLn (show e')
        return ()

stepWithRendering :: HACSimHandle -> Double -> IO ()
stepWithRendering hdl dt = GLO.simulateIO Front.display
                                GLO.white
                                30
                                hdl
                                modelToPicture
                                (stepIteration dt)

modelToPicture :: HACSimHandle -> IO GLO.Picture
modelToPicture hdl = return (Front.renderFrame observableAgentStates)
    where
        as = PA.extractHdlAgents hdl
        observableAgentStates = map hacAgentToObservableState as

stepIteration :: Double -> GLO.ViewPort -> Float -> HACSimHandle -> IO HACSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)

hacAgentToObservableState :: HACAgent -> (Double, Double, Bool)
hacAgentToObservableState a = (x, y, h)
    where
        s = PA.state a
        (x, y) = pos s
        h = hero s