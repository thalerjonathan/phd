module Segregation.Run (
    runSegWithRendering,
    runSegStepsAndRender
  ) where

import Segregation.Model
import Segregation.Init
import Segregation.Stats
import Segregation.Renderer as Renderer

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Init
import FrABS.Rendering.GlossSimulator

import Text.Printf

import System.IO

import Data.Maybe

winSize = (800, 800)
winTitle = "Schelling Segregation"
rngSeed = 42
cells = (10, 10)
agentCount = 10
samplingTimeDelta = 1.0
frequency = 0
steps = 10
updateStrat = Sequential
envCollapsing = Nothing
shuffleAgents = True

runSegWithRendering :: IO ()
runSegWithRendering = 
    do
        params <- initSimulation updateStrat envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSegregation cells
        
        putStrLn "dynamics = ["

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            (Just printDynamics)

runSegStepsAndRender :: IO ()
runSegStepsAndRender = 
    do
        params <- initSimulation updateStrat envCollapsing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSegregation cells
        
        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

printDynamics :: ([SegAgentOut], SegEnvironment)
                    ->([SegAgentOut], SegEnvironment)
                    -> IO ()
printDynamics (aoutsPrev, _) (aoutsCurr, _) = 
    do
        let maxSimilarity = fromInteger $ fromIntegral totalCount -- NOTE: an agent can reach a maximum of 1.0
        let currSimilarity = totalSatisfaction aoutsCurr
        let prevSimilarity = totalSatisfaction aoutsPrev
        let similarityDelta = currSimilarity - prevSimilarity

        let currSimilarityNormalized = currSimilarity / maxSimilarity
        let similarityDeltaNormalized = similarityDelta / maxSimilarity

        putStrLn (printf "%.3f" unhappyFract 
                    ++ "," ++ printf "%.3f" currSimilarityNormalized
                    ++ "," ++ printf "%.3f" similarityDeltaNormalized
                    ++ ";")
        where
            (totalCount, _, _, unhappyFract) = satisfactionStats aoutsCurr

