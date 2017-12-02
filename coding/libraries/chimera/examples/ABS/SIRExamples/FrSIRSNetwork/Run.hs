module FrSIRSNetwork.Run ( 
    runFrSIRSNetworkWithRendering,
    runFrSIRSNetworkStepsAndRender,

    runFrSIRSNetworkStepsAndWriteToFile,
    runFrSIRSNetworkReplicationsAndWriteToFile
  ) where

import FRP.Yampa

import FrSIRSNetwork.Init
import FrSIRSNetwork.Model
import FrSIRSNetwork.Renderer
import Utils.Sir

import FRP.FrABS

winSize = (800, 800)
winTitle = "FrSIRS Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

dt = 0.05
t = 150

replCfg = ReplicationConfig {
    replCfgCount = 4,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

--agentCount = 32 * 32 :: Int
agentCount = 1000
numInfected = 10

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 3
barbasiAlbertM = 1

network = Deterministic completeNetwork

-- TODO: repair

runFrSIRSNetworkWithRendering :: IO ()
runFrSIRSNetworkWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        -- (initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            dt
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
                            dt
                            t
                            winTitle
                            winSize
                            renderFrSIRSNetworkFrame

runFrSIRSNetworkStepsAndWriteToFile :: IO ()
runFrSIRSNetworkStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network

        let asenv = simulateTime initAdefs initEnv params dt t
        let dynamics = map calculateDynamics asenv
        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentCount ++ "agents_" 
                        ++ show t ++ "time_" 
                        ++ show dt ++ "dt.m"

        writeSirDynamicsFile fileName dt 0 dynamics

runFrSIRSNetworkReplicationsAndWriteToFile :: IO ()
runFrSIRSNetworkReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        
        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected initialInfectionProb network

        let assenv = runReplications initAdefs initEnv params dt t replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirDynamicsReplMean replicationDynamics


        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentCount ++ "agents_" 
                        ++ show t ++ "time_" 
                        ++ show dt ++ "dt_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeSirDynamicsFile fileName dt (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
calculateDynamics :: (Time, [FrSIRSNetworkAgentObservable], FrSIRSNetworkEnvironment) -> (Time, Double, Double, Double)
calculateDynamics (t, aobs, _) = (t, susceptibleCount, infectedCount, recoveredCount)
    where
        susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
        infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
        recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs

        totalCount = susceptibleCount + infectedCount + recoveredCount :: Double

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount

calculateSingleReplicationDynamic :: [(Time, [FrSIRSNetworkAgentObservable], FrSIRSNetworkEnvironment)] -> [(Time, Double, Double, Double)]
calculateSingleReplicationDynamic = map calculateDynamics