module Main 
  ( 
    main

  , runFrSIRStepsAndWriteToFile
  , runFrSIRDeltasAndWriteToFile
  , runFrSIRReplicationsAndWriteToFile
  ) where

import FRP.Chimera
import FRP.Yampa

import Init
import Model
import Sir

updateStrat :: UpdateStrategy
updateStrat = Parallel

shuffleAgents :: Bool
shuffleAgents = False

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.1

t :: DTime
t = 150

agentCount :: Int
agentCount = 1000

numInfected :: Int
numInfected = 10

replCfg :: FrSIRReplicationConfig
replCfg = ReplicationConfig 
  {
    replCfgCount = 4
  , replCfgAgentReplicator = (sirAgentDefReplicator numInfected)
  , replCfgEnvReplicator = defaultEnvReplicator
  }

main :: IO () 
main = runFrSIRStepsAndWriteToFile

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

  writeSirDynamicsFile fileName dt 0 dynamics

runFrSIRDeltasAndWriteToFile :: IO ()
runFrSIRDeltasAndWriteToFile = do
  params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
  
  (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected
  
  let deltasBefore = replicate 50 dt
  let deltasZero = replicate 50 0 -- NOTE: this is like halting time, SIR agents won't change as they completely rely on advancing time
  let deltasAfter = replicate 100 dt
  let deltas = deltasZero ++ deltasBefore ++ deltasAfter

  let dynamics = simulateAggregateTimeDeltas initAdefs initEnv params deltas aggregate
  let fileName = "frSIRDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt_"
                  ++ show updateStrat ++ ".m"

  writeSirDynamicsFile fileName dt 0 dynamics

runFrSIRReplicationsAndWriteToFile :: IO ()
runFrSIRReplicationsAndWriteToFile = do
  params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
  
  (initAdefs, initEnv) <- createFrSIRNumInfected agentCount numInfected

  let replicationDynamics = runReplicationsWithAggregation initAdefs initEnv params dt t replCfg aggregate
  let dynamics = sirDynamicsReplMean replicationDynamics

  let fileName = "frSIRDynamics_" 
                  ++ show agentCount ++ "agents_" 
                  ++ show t ++ "time_"
                  ++ show dt ++ "dt_"
                  ++ show updateStrat ++ "_"
                  ++ show (replCfgCount replCfg) ++ "replications.m"

  writeSirDynamicsFile fileName dt (replCfgCount replCfg) dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
aggregate :: (Time, [FrSIRAgentObservable], FrSIREnvironment) -> (Time, Double, Double, Double)
aggregate (t, aobs, _) = (t, susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . snd) aobs
    infectedCount = fromIntegral $ length $ filter ((Infected==) . snd) aobs
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . snd) aobs