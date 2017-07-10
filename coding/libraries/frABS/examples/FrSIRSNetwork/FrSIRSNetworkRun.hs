module FrSIRSNetwork.FrSIRSNetworkRun ( 
    runFrSIRSNetworkWithRendering,
    runFrSIRSNetworkStepsAndRender,

    runFrSIRSNetworkStepsAndWriteToFile,
    runFrSIRSNetworkReplicationsAndWriteToFile
  ) where

import FrSIRSNetwork.FrSIRSNetworkInit
import FrSIRSNetwork.FrSIRSNetworkModel
import FrSIRSNetwork.FrSIRSNetworkRenderer as Renderer
import Utils.Sirs

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Utils
import FrABS.Rendering.GlossSimulator
import FrABS.Env.Utils

import Text.Printf
import System.IO
import Debug.Trace 

winSize = (800, 800)
winTitle = "FrSIRS Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (32, 32)
agentCount = fst agentDimensions * snd agentDimensions

numInfected = 1

samplingTimeDelta = 0.1
steps = 1000
replications = 4

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 3
barbasiAlbertM = 1

network = completeNetwork

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
        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt.m"

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

        let fileName = "frSIRSNetworkDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show samplingTimeDelta ++ "dt_" 
                        ++ show replications ++ "replications.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta replications dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
calculateDynamics :: [FrSIRSNetworkAgentOut] -> (Double, Double, Double)
calculateDynamics aos = (susceptibleCount, infectedCount, recoveredCount)
    where
        susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . aoState) aos
        infectedCount = fromIntegral $ length $ filter ((Infected==) . aoState) aos
        recoveredCount = fromIntegral $ length $ filter ((Recovered==) . aoState) aos

        totalCount = susceptibleCount + infectedCount + recoveredCount :: Double

        susceptibleRatio = susceptibleCount / totalCount
        infectedRatio = infectedCount / totalCount 
        recoveredRatio = recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSNetworkAgentOut], FrSIRSNetworkEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic = map (calculateDynamics . fst)