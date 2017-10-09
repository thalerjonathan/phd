module FrSIR.Run 
    ( 
      runFrSIRStepsAndWriteToFile
    , runFrSIRReplicationsAndWriteToFile
    ) where

import FRP.Yampa
import FRP.FrABS

import FrSIR.Init
import FrSIR.Model
import Utils.Sirs

updateStrat :: UpdateStrategy
updateStrat = Parallel

shuffleAgents :: Bool
shuffleAgents = False

rngSeed :: Int
rngSeed = 42

samplingTimeDelta :: DTime
samplingTimeDelta = 0.05

steps :: Int
steps = 3000

agentCount :: Int
agentCount = 1000

numInfected :: Int
numInfected = 10

replCfg :: FrSIRReplicationConfig
replCfg = ReplicationConfig {
    replCfgCount = 20,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

runFrSIRStepsAndWriteToFile :: IO ()
runFrSIRStepsAndWriteToFile = do
    params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
    
    (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected
    
    let dynamics = processAndAggregateSteps initAdefs initEnv params samplingTimeDelta steps aggregate
    let fileName = "frSIRDynamics_" 
                    ++ show agentCount ++ "agents_" 
                    ++ show steps ++ "steps_" 
                    ++ show samplingTimeDelta ++ "dt.m"

    writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRReplicationsAndWriteToFile :: IO ()
runFrSIRReplicationsAndWriteToFile = do
    params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
    
    (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected

    let replicationDynamics = runReplicationsWithAggregation initAdefs initEnv params samplingTimeDelta steps replCfg aggregate
    let dynamics = sirsDynamicsReplMean replicationDynamics

    let fileName = "frSIRDynamics_" 
                    ++ show agentCount ++ "agents_" 
                    ++ show steps ++ "steps_" 
                    ++ show samplingTimeDelta ++ "dt_" 
                    ++ show (replCfgCount replCfg) ++ "replications.m"

    writeSirsDynamicsFile fileName steps samplingTimeDelta (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
aggregate :: ([FrSIRAgentObservable], FrSIREnvironment) -> (Double, Double, Double)
aggregate (aobs, _) = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
    infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs