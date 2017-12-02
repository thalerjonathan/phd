module Segregation.Run (
    runSegWithRendering,
    runSegStepsAndRender
  ) where

import FRP.Yampa

import Segregation.Model
import Segregation.Init
import Segregation.Stats
import Segregation.Renderer

import FRP.FrABS

import Text.Printf

winSize = (800, 800)
winTitle = "Schelling Segregation"
rngSeed = 42
cells = (10, 10)
samplingTimeDelta = 1.0
frequency = 0
steps = 10
updateStrat = Sequential
shuffleAgents = True

-- TODO: repair

runSegWithRendering :: IO ()
runSegWithRendering = 
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSegregation cells
        
        putStrLn "dynamics = ["

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderSegFrame
                            (Just printDynamics)

runSegStepsAndRender :: IO ()
runSegStepsAndRender = 
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createSegregation cells
        
        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            renderSegFrame

printDynamics :: (Time, [(AgentId, SegAgentState)], SegEnvironment)
                    ->(Time, [(AgentId, SegAgentState)], SegEnvironment)
                    -> IO ()
printDynamics (_, aoutsPrev, _) (_, aoutsCurr, _) = 
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

