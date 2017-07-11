module FrSIRSSpatial.FrSIRSSpatialRun ( 
    runFrSIRSSpatialWithRendering,
    runFrSIRSSpatialStepsAndRender,
    runFrSIRSSpatialStepsAndWriteToFile,
    runFrSIRSSpatialReplicationsAndWriteToFile
  ) where

import FrSIRSSpatial.FrSIRSSpatialInit
import FrSIRSSpatial.FrSIRSSpatialModel
import FrSIRSSpatial.FrSIRSSpatialRenderer as Renderer
import Utils.Sirs

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Init
import FrABS.Simulation.Replication
import FrABS.Rendering.GlossSimulator

import Text.Printf
import System.IO
import Debug.Trace 

winSize = (800, 800)
winTitle = "FrSIRS Spatial"

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (32, 32)
frequency = 0

samplingTimeDelta = 0.1
steps = 3500

replCfg = ReplicationConfig {
    replCfgCount = 4,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

runFrSIRSSpatialWithRendering :: IO ()
runFrSIRSSpatialWithRendering =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing --(Just (\_ asenv -> printAgentDynamics asenv))

runFrSIRSSpatialStepsAndRender :: IO ()
runFrSIRSSpatialStepsAndRender =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

runFrSIRSSpatialStepsAndWriteToFile :: IO ()
runFrSIRSSpatialStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map agentsToDynamics asenv
        let fileName = "frSIRSSpatialDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRSSpatialReplicationsAndWriteToFile :: IO ()
runFrSIRSSpatialReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics

        let fileName = "frSIRSSpatialDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
agentsToDynamics = (calculateDynamics . fst)
printAgentDynamics = (putStrLn . sirsDynamicToString . agentsToDynamics)

calculateDynamics :: [FrSIRSSpatialAgentOut] -> (Double, Double, Double)
calculateDynamics aos = (susceptibleCount, infectedCount, recoveredCount)
    where
        susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . aoState) aos
        infectedCount = fromIntegral $ length $ filter ((Infected==) . aoState) aos
        recoveredCount = fromIntegral $ length $ filter ((Recovered==) . aoState) aos

        totalCount = susceptibleCount + infectedCount + recoveredCount

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSSpatialAgentOut], FrSIRSSpatialEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic  aoss = map (calculateDynamics . fst) aoss
