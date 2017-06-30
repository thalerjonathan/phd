module FrSIRSNetwork.FrSIRSNetworkRun ( 
    runFrSIRSNetworkStepsAndWriteToFile,
    runFrSIRSNetworkReplicationsAndWriteToFile
  ) where

import FrSIRSNetwork.FrSIRSNetworkInit
import FrSIRSNetwork.FrSIRSNetworkModel
import Utils.Utils

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils

import Text.Printf
import System.IO
import Debug.Trace 

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (51, 51)
numInfected = 1

samplingTimeDelta = 0.1
steps = 2000
replications = 10

runFrSIRSNetworkStepsAndWriteToFile :: IO ()
runFrSIRSNetworkStepsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRSNetworkFullConnectedNumInfected agentDimensions numInfected
        params <- initSimParams updateStrat Nothing shuffleAgents

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map (calculateDynamics . fst) asenv
        let fileName = "frSIRSNetworkDynamics_" ++ (show agentDimensions) ++ "agents_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRSNetworkReplicationsAndWriteToFile :: IO ()
runFrSIRSNetworkReplicationsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRSNetworkFullConnectedNumInfected agentDimensions numInfected
        params <- initSimParams updateStrat Nothing shuffleAgents

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replications
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics

        let fileName = "frSIRSNetworkDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt_" ++ (show replications) ++ "_replications.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta replications dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
calculateDynamics :: [FrSIRSNetworkAgentOut] -> (Double, Double, Double)
calculateDynamics aos = (susceptibleRatio, infectedRatio, recoveredRatio)
    where
        susceptibleCount = length $ filter ((Susceptible==) . aoState) aos
        infectedCount = length $ filter ((Infected==) . aoState) aos
        recoveredCount = length $ filter ((Recovered==) . aoState) aos

        totalCount = fromIntegral $ susceptibleCount + infectedCount + recoveredCount :: Double

        susceptibleRatio = fromIntegral susceptibleCount / totalCount
        infectedRatio = fromIntegral infectedCount / totalCount 
        recoveredRatio = fromIntegral recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSNetworkAgentOut], FrSIRSNetworkEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic  aoss = map (calculateDynamics . fst) aoss