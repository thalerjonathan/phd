module FrSIRS.FrSIRSRun ( 
    runFrSIRSWithRendering,
    runFrSIRSStepsAndRender,
    runFrSIRSStepsAndWriteToFile,
    runFrSIRSReplicationsAndWriteToFile
  ) where

import FrSIRS.FrSIRSInit
import FrSIRS.FrSIRSModel
import FrSIRS.FrSIRSRenderer as Renderer
import Utils.Utils

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.SimulationUtils
import FrABS.Rendering.GlossSimulator

import Text.Printf
import System.IO
import Debug.Trace 

winSize = (800, 800)
winTitle = "FrSIRS"

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (51, 51)
frequency = 0

samplingTimeDelta = 1.0
steps = 1000
replications = 10

runFrSIRSWithRendering :: IO ()
runFrSIRSWithRendering =
    do
        _ <- initRng rngSeed
        -- (initAdefs, initEnv) <- createFrSIRSRandomInfected agentDimensions initialInfectionProb
        (initAdefs, initEnv) <- createFrSIRSSingleInfected agentDimensions
        
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            (Just (\_ asenv -> printAgentDynamics asenv))

runFrSIRSStepsAndRender :: IO ()
runFrSIRSStepsAndRender =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRSRandomInfected agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

runFrSIRSStepsAndWriteToFile :: IO ()
runFrSIRSStepsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRSRandomInfected agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map agentsToDynamics asenv
        let fileName = "frSIRSDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta 0 dynamics

runFrSIRSReplicationsAndWriteToFile :: IO ()
runFrSIRSReplicationsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRSRandomInfected agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replications
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = sirsDynamicsReplMean replicationDynamics

        let fileName = "frSIRSDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt_" ++ (show replications) ++ "_replications.m"

        writeSirsDynamicsFile fileName steps samplingTimeDelta replications dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
agentsToDynamics = (calculateDynamics . fst)
printAgentDynamics = (putStrLn . sirsDynamicToString . agentsToDynamics)

calculateDynamics :: [FrSIRSAgentOut] -> (Double, Double, Double)
calculateDynamics aos = (susceptibleRatio, infectedRatio, recoveredRatio)
    where
        susceptibleCount = length $ filter ((Susceptible==) . aoState) aos
        infectedCount = length $ filter ((Infected==) . aoState) aos
        recoveredCount = length $ filter ((Recovered==) . aoState) aos

        totalCount = fromIntegral $ susceptibleCount + infectedCount + recoveredCount :: Double

        susceptibleRatio = fromIntegral susceptibleCount / totalCount
        infectedRatio = fromIntegral infectedCount / totalCount 
        recoveredRatio = fromIntegral recoveredCount / totalCount

calculateSingleReplicationDynamic :: [([FrSIRSAgentOut], FrSIRSEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic  aoss = map (calculateDynamics . fst) aoss
