module FrSIRSSpatial.Run ( 
    runFrSIRSSpatialWithRendering,
    runFrSIRSSpatialStepsAndRender,
    runFrSIRSSpatialStepsAndWriteToFile,
    runFrSIRSSpatialReplicationsAndWriteToFile,

    debugFrSIRSSpatialWithRendering
  ) where

import FrSIRSSpatial.Init
import FrSIRSSpatial.Model
import FrSIRSSpatial.Renderer
import Utils.Sirs

import FRP.FrABS

winSize = (800, 800)
winTitle = "FrSIRS Spatial"

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (52, 52)
frequency = 0

dt = 0.1
t = 150

replCfg = ReplicationConfig {
    replCfgCount = 4,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

runFrSIRSSpatialWithRendering :: IO ()
runFrSIRSSpatialWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            dt
                            frequency
                            winTitle
                            winSize
                            renderFrSIRSSpatialFrame
                            Nothing --(Just (\_ asenv -> printAgentDynamics asenv))

debugFrSIRSSpatialWithRendering :: IO ()
debugFrSIRSSpatialWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions
        
        debugAndRender initAdefs
                            initEnv
                            params
                            dt
                            frequency
                            winTitle
                            winSize
                            renderFrSIRSSpatialFrame

runFrSIRSSpatialStepsAndRender :: IO ()
runFrSIRSSpatialStepsAndRender =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            dt
                            t
                            winTitle
                            winSize
                            renderFrSIRSSpatialFrame

runFrSIRSSpatialStepsAndWriteToFile :: IO ()
runFrSIRSSpatialStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        let asenv = simulateTime initAdefs initEnv params dt t
        let dynamics = map agentsToDynamics asenv
        let fileName = "frSIRSSpatialDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show t ++ "time_" 
                        ++ show dt ++ "dt.m"

        writeSirsDynamicsFile fileName dt 0 dynamics

runFrSIRSSpatialReplicationsAndWriteToFile :: IO ()
runFrSIRSSpatialReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)

        -- (initAdefs, initEnv) <- createFrSIRSSpatialRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSpatialSingleInfected agentDimensions

        let assenv = runReplications initAdefs initEnv params dt t replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics

        let fileName = "frSIRSSpatialDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show t ++ "time_" 
                        ++ show dt ++ "dt_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeSirsDynamicsFile fileName dt (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
agentsToDynamics = (calculateDynamics . fst)
printAgentDynamics = (putStrLn . sirsDynamicToString . agentsToDynamics)

calculateDynamics :: [FrSIRSSpatialAgentObservable] -> (Double, Double, Double)
calculateDynamics aobs = (susceptibleCount, infectedCount, recoveredCount)
    where
        susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . sirsState . snd) aobs
        infectedCount = fromIntegral $ length $ filter ((Infected==) . sirsState . snd) aobs
        recoveredCount = fromIntegral $ length $ filter ((Recovered==) . sirsState . snd) aobs

        totalCount = susceptibleCount + infectedCount + recoveredCount

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSSpatialAgentObservable], FrSIRSSpatialEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic = map (calculateDynamics . fst)
