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
updateStrat = Sequential

shuffleAgents :: Bool
shuffleAgents = True

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 1.0

t :: DTime
t = 150

agentCount :: Int
agentCount = 1000

numInfected :: Int
numInfected = 10

replCfg :: FrSIRReplicationConfig
replCfg = ReplicationConfig {
    replCfgCount = 10,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

runFrSIRStepsAndWriteToFile :: IO ()
runFrSIRStepsAndWriteToFile = do
    params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
    
    (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected
    
    let dynamics = simulateAggregateTime initAdefs initEnv params dt t aggregate
    let fileName = "frSIRDynamics_" 
                    ++ show agentCount ++ "agents_" 
                    ++ show t ++ "time_"
                    ++ show dt ++ "dt_"
                    ++ show updateStrat ++ ".m"

    writeSirsDynamicsFile fileName dt 0 dynamics

runFrSIRReplicationsAndWriteToFile :: IO ()
runFrSIRReplicationsAndWriteToFile = do
    params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
    
    (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected

    let replicationDynamics = runReplicationsWithAggregation initAdefs initEnv params dt t replCfg aggregate
    let dynamics = sirsDynamicsReplMean replicationDynamics

    let fileName = "frSIRDynamics_" 
                    ++ show agentCount ++ "agents_" 
                    ++ show t ++ "time_"
                    ++ show dt ++ "dt_"
                    ++ show updateStrat ++ "_"
                    ++ show (replCfgCount replCfg) ++ "replications.m"

    writeSirsDynamicsFile fileName dt (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
aggregate :: ([FrSIRAgentObservable], FrSIREnvironment) -> (Double, Double, Double)
aggregate (aobs, _) = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
    infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs