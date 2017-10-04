module FrSIRSNetwork.Run ( 
    runFrSIRSNetworkWithRendering,
    runFrSIRSNetworkStepsAndRender,

    runFrSIRSNetworkStepsAndWriteToFile,
    runFrSIRSNetworkReplicationsAndWriteToFile
  ) where

import FrSIRSNetwork.Init
import FrSIRSNetwork.Model
import FrSIRSNetwork.Renderer
import Utils.Sirs

import FRP.FrABS

winSize = (800, 800)
winTitle = "FrSIRS Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

samplingTimeDelta = 0.1
steps = 1000

replCfg = ReplicationConfig {
    replCfgCount = 8,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator,
    replCfgFilter = Just validReplication
}

validReplication :: Replication FrSIRSNetworkAgentState FrSIRSNetworkEnvironment -> Bool
validReplication repl = True -- TODO: any > 1
    where
        recoveredCounts = map (countRecovered . fst) repl

        countRecovered :: [FrSIRSNetworkAgentObservable] -> Int
        countRecovered aobs = fromIntegral $ length $ filter ((Recovered==) . snd) aobs

--agentCount = 32 * 32 :: Int
agentCount = 1000
numInfected = 1

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 3
barbasiAlbertM = 1

network = completeNetwork

runFrSIRSNetworkWithRendering :: IO ()
runFrSIRSNetworkWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        -- (initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderFrSIRSNetworkFrame
                            Nothing

runFrSIRSNetworkStepsAndRender :: IO ()
runFrSIRSNetworkStepsAndRender =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network
        
        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            renderFrSIRSNetworkFrame

runFrSIRSNetworkStepsAndWriteToFile :: IO ()
runFrSIRSNetworkStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map (calculateDynamics . fst) asenv
        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentCount ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRSNetworkReplicationsAndWriteToFile :: IO ()
runFrSIRSNetworkReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics


        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentCount ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
calculateDynamics :: [FrSIRSNetworkAgentObservable] -> (Double, Double, Double)
calculateDynamics aobs = (susceptibleCount, infectedCount, recoveredCount)
    where
        susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
        infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
        recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs

        totalCount = susceptibleCount + infectedCount + recoveredCount :: Double

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSNetworkAgentObservable], FrSIRSNetworkEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic = map (calculateDynamics . fst)