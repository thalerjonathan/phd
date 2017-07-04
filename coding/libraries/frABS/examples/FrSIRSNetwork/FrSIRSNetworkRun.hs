module FrSIRSNetwork.FrSIRSNetworkRun ( 
    runFrSIRSNetworkWithRendering,
    runFrSIRSNetworkStepsAndRender,

    runFrSIRSNetworkStepsAndWriteToFile,
    runFrSIRSNetworkReplicationsAndWriteToFile
  ) where

import FrSIRSNetwork.FrSIRSNetworkInit
import FrSIRSNetwork.FrSIRSNetworkModel
import FrSIRSNetwork.FrSIRSNetworkRenderer as Renderer
import Utils.Utils

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator
import FrABS.Env.EnvironmentUtils

import Text.Printf
import System.IO
import Debug.Trace 

winSize = (800, 800)
winTitle = "FrSIRS Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (31, 31)
agentCount = fst agentDimensions * snd agentDimensions

numInfected = 1

samplingTimeDelta = 0.1
steps = 1500
replications = 10

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 100
barbasiAlbertM = 10

network = barbasiAlbertNetwork

runFrSIRSNetworkWithRendering :: IO ()
runFrSIRSNetworkWithRendering =
    do
        _ <- initRng rngSeed

        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected agentDimensions numInfected network
        -- (initAdefs, initEnv) <- createFrSIRSNetworkRandInfected agentDimensions initialInfectionProb network
        
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing

runFrSIRSNetworkStepsAndRender :: IO ()
runFrSIRSNetworkStepsAndRender =
    do
        _ <- initRng rngSeed

        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected agentDimensions numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected agentDimensions initialInfectionProb network
        
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

runFrSIRSNetworkStepsAndWriteToFile :: IO ()
runFrSIRSNetworkStepsAndWriteToFile =
    do
        _ <- initRng rngSeed

        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected agentDimensions numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected agentDimensions initialInfectionProb network

        params <- initSimParams updateStrat Nothing shuffleAgents

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map (calculateDynamics . fst) asenv
        let fileName = "frSIRSNetworkDynamics_" ++ show agentDimensions ++ "agents_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRSNetworkReplicationsAndWriteToFile :: IO ()
runFrSIRSNetworkReplicationsAndWriteToFile =
    do
        _ <- initRng rngSeed

        (initAdefs, initEnv) <- createFrSIRSNetworkNumInfected agentDimensions numInfected network
        --(initAdefs, initEnv) <- createFrSIRSNetworkRandInfected agentDimensions initialInfectionProb network

        params <- initSimParams updateStrat Nothing shuffleAgents

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replications
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics

        let fileName = "frSIRSNetworkDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt_" ++ show replications ++ "_replications.m"

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
calculateSingleReplicationDynamic = map (calculateDynamics . fst)