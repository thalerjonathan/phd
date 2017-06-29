module FrSIRS.FrSIRSRun ( 
    runFrSIRSWithRendering,
    runFrSIRSStepsAndRender,
    runFrSIRSStepsAndPrint,
    runFrSIRSStepsAndWriteToFile,
    runFrSIRSReplicationsAndWriteToFile
  ) where

import FrSIRS.FrSIRSInit
import FrSIRS.FrSIRSModel
import FrSIRS.FrSIRSRenderer as Renderer

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
        (initAdefs, initEnv) <- createFrSIRS agentDimensions initialInfectionProb
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
        (initAdefs, initEnv) <- createFrSIRS agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        simulateStepsAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            steps
                            winTitle
                            winSize
                            Renderer.renderFrame

runFrSIRSStepsAndPrint :: IO ()
runFrSIRSStepsAndPrint =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRS agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps

        putStrLn ("steps = " ++ show steps ++ ";")
        putStrLn ("dt = " ++ show samplingTimeDelta ++ ";")
        putStrLn "dynamics = ["
        mapM_ printAgentDynamics asenv
        putStrLn "];"

        return ()

runFrSIRSStepsAndWriteToFile :: IO ()
runFrSIRSStepsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRS agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map agentsToDynamics asenv
        let fileName = "frSIRSDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt.m"

        writeDynamicsFile fileName dynamics

runFrSIRSReplicationsAndWriteToFile :: IO ()
runFrSIRSReplicationsAndWriteToFile =
    do
        _ <- initRng rngSeed
        (initAdefs, initEnv) <- createFrSIRS agentDimensions initialInfectionProb
        params <- initSimParams updateStrat Nothing shuffleAgents

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replications
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = calculateDynamicMean replicationDynamics

        let fileName = "frSIRSDynamics_" ++ show steps ++ "steps_" ++ show samplingTimeDelta ++ "_dt_" ++ (show replications) ++ "_replications.m"

        writeDynamicsFile fileName dynamics

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
writeDynamicsFile :: String -> [(Double, Double, Double)] -> IO ()
writeDynamicsFile fileName dynamics =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("steps = " ++ show steps ++ ";")
        hPutStrLn fileHdl ("dt = " ++ show samplingTimeDelta ++ ";")
        hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")
        
        hPutStrLn fileHdl "dynamics = ["
        mapM_ (hPutStrLn fileHdl . dynamicsToString) dynamics
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "susceptibleRatio = dynamics (:, 1);"
        hPutStrLn fileHdl "infectedRatio = dynamics (:, 2);"
        hPutStrLn fileHdl "recoveredRatio = dynamics (:, 3);"
        hPutStrLn fileHdl "figure"
        hPutStrLn fileHdl "plot (susceptibleRatio.', 'color', 'green');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (infectedRatio.', 'color', 'red');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (recoveredRatio.', 'color', 'blue');"
        hPutStrLn fileHdl "xlabel ('Steps');"
        hPutStrLn fileHdl "ylabel ('Ratio');"
        hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"
        hPutStrLn fileHdl ("title ('SIRS Dynamics with " ++ show samplingTimeDelta ++ " dt, " ++ show steps ++ " steps, " ++  (show replications) ++ " replications');")

        hClose fileHdl

agentsToDynamics = (calculateDynamics . fst)
printAgentDynamics = (putStrLn . dynamicsToString . agentsToDynamics)

dynamicsToString :: (Double, Double, Double) -> String
dynamicsToString dynamics = 
                printf "%.3f" susceptibleRatio 
                    ++ "," ++ printf "%.3f" infectedRatio
                    ++ "," ++ printf "%.3f" recoveredRatio
                    ++ ";" 
    where
        (susceptibleRatio, infectedRatio, recoveredRatio) = dynamics 

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

calculateDynamicMean :: [[(Double, Double, Double)]] -> [(Double, Double, Double)]
calculateDynamicMean [] = []
calculateDynamicMean replDynamics@(initRepl:tailRepls) = replDynamicsRatio
    where
        replCountRational = fromIntegral $ length replDynamics :: Double

        replDynamicsSum = foldr sumDyns initRepl tailRepls
        replDynamicsRatio = map (\(s, i, r) -> (s / replCountRational, i / replCountRational, r / replCountRational)) replDynamicsSum

        sumDyns :: [(Double, Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
        sumDyns ds1 ds2 = map (\((s1, i1, r1), (s2, i2, r2)) -> (s1+s2, i1+i2, r1+r2)) (zip ds1 ds2)