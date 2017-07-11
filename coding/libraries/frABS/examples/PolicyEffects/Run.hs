module PolicyEffects.Run ( 
    runPolicyEffectsWithRendering,
    runPolicyEffectsStepsAndWriteToFile,
    runPolicyEffectsReplicationsAndWriteToFile
  ) where

import PolicyEffects.Init
import PolicyEffects.Model
import PolicyEffects.Renderer as Renderer

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Init
import FrABS.Simulation.Replication
import FrABS.Rendering.GlossSimulator
import FrABS.Env.Utils

import Text.Printf
import System.IO
import Debug.Trace 

winSize = (800, 800)
winTitle = "Policy Effects Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentDimensions = (10, 10)
agentCount = fst agentDimensions * snd agentDimensions

initialWealth :: Double
initialWealth = 100

samplingTimeDelta = 1.0
steps = 10000

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 3
barbasiAlbertM = 1

network = completeNetwork

replCfg = ReplicationConfig {
    replCfgCount = 8,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = defaultEnvReplicator
}

runPolicyEffectsWithRendering :: IO ()
runPolicyEffectsWithRendering =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects agentDimensions initialWealth network
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            Renderer.renderFrame
                            Nothing

runPolicyEffectsStepsAndWriteToFile :: IO ()
runPolicyEffectsStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects agentDimensions initialWealth network

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps
        let dynamics = map (calculateDynamics . fst) asenv
        let dynamicsFileName = "policyEffectsDynamics_" 
                                ++ show agentDimensions ++ "agents_" 
                                ++ show steps ++ "steps.m" 

        let finalAgents = (fst . last) asenv
        let histFileName = "policyEffectsHistogram_" 
                                ++ show agentDimensions ++ "agents_" 
                                ++ show steps ++ "steps.m" 

        writeDynamicsFile dynamicsFileName steps 0 dynamics
        writeHistogramFile histFileName steps 0 finalAgents

runPolicyEffectsReplicationsAndWriteToFile :: IO ()
runPolicyEffectsReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects agentDimensions initialWealth network

        let assenv = runReplications initAdefs initEnv params samplingTimeDelta steps replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = dynamicsReplMean replicationDynamics

        let fileName = "policyEffectsDynamics_" 
                        ++ show agentDimensions ++ "agents_" 
                        ++ show steps ++ "steps_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeDynamicsFile fileName steps (replCfgCount replCfg) dynamics


calculateDynamics :: [PolicyEffectsAgentOut] -> (Double, Double, Double)
calculateDynamics aos = (minWealth, maxWealth, std)
    where
        n = length aos
        s = foldr (\ao m -> aoState ao + m) 0 aos

        maxWealth = foldr (\ao m -> if (aoState ao > m) then aoState ao else m) 0 aos
        minWealth = foldr (\ao m -> if (aoState ao < m) then aoState ao else m) (initialWealth * fromIntegral agentCount) aos

        mean = s / fromIntegral n
        dev = sum $ map (\ao -> (aoState ao - mean)^2) aos
        std = sqrt dev

calculateSingleReplicationDynamic :: [([PolicyEffectsAgentOut], PolicyEffectsEnvironment)] -> [(Double, Double, Double)]
calculateSingleReplicationDynamic = map (calculateDynamics . fst)

dynamicsReplMean :: [[(Double, Double, Double)]] -> [(Double, Double, Double)]
dynamicsReplMean [] = []
dynamicsReplMean replDynamics@(initRepl:tailRepls) = replDynamicsRatio
    where
        replCountRational = fromIntegral $ length replDynamics :: Double

        replDynamicsSum = foldr sumDyns initRepl tailRepls
        replDynamicsRatio = map (\(minWealth, maxWealth, std) -> (minWealth / replCountRational, maxWealth / replCountRational, std / replCountRational)) replDynamicsSum

        sumDyns :: [(Double, Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
        sumDyns ds1 ds2 = map (\((m1, s1, x1), (m2, s2, x2)) -> (m1+m2, s1+s2, x1+x2)) (zip ds1 ds2)

dynamicsToString :: (Double, Double, Double) -> String
dynamicsToString dynamics = 
                printf "%.3f" minWealth 
                    ++ "," ++ printf "%.3f" maxWealth
                    ++ "," ++ printf "%.3f" std
                    ++ ";" 
    where
        (minWealth, maxWealth, std) = dynamics 

agentWealthToString :: PolicyEffectsAgentOut -> String
agentWealthToString ao = 
                printf "%.1f" wealth ++ ";" 
    where
        wealth = aoState ao

writeDynamicsFile :: String 
                        -> Int
                        -> Int
                        -> [(Double, Double, Double)] -> IO ()
writeDynamicsFile fileName steps replications dynamics =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("steps = " ++ show steps ++ ";")
        hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")
        
        hPutStrLn fileHdl "dynamics = ["
        mapM_ (hPutStrLn fileHdl . dynamicsToString) dynamics
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "minWealth = dynamics (:, 1);"
        hPutStrLn fileHdl "maxWealth = dynamics (:, 2);"
        hPutStrLn fileHdl "std = dynamics (:, 3);"

        hPutStrLn fileHdl "figure"
        hPutStrLn fileHdl "plot (minWealth.', 'color', 'red');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (maxWealth.', 'color', 'green');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (std.', 'color', 'blue');"
        hPutStrLn fileHdl "xlabel ('Time');"
        hPutStrLn fileHdl "ylabel ('Wealth');"
        hPutStrLn fileHdl "legend('Min Wealth','Max Wealth', 'Std');"
        hPutStrLn fileHdl ("title ('Policy Effects over " ++ show steps ++ " steps, " ++  (show replications) ++ " replications');")

        hClose fileHdl

writeHistogramFile :: String 
                        -> Int
                        -> Int
                        -> [PolicyEffectsAgentOut] -> IO ()
writeHistogramFile fileName steps replications aos =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("steps = " ++ show steps ++ ";")
        hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")
        
        hPutStrLn fileHdl "agentsWealth = ["
        mapM_ (hPutStrLn fileHdl . agentWealthToString) aos
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "hist(agentsWealth);"
        
        hClose fileHdl