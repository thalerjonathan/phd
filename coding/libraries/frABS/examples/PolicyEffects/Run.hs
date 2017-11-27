module PolicyEffects.Run ( 
    runPolicyEffectsWithRendering,
    runPolicyEffectsStepsAndWriteToFile,
    runPolicyEffectsReplicationsAndWriteToFile
  ) where

import FRP.Yampa

import PolicyEffects.Init
import PolicyEffects.Model
import PolicyEffects.Renderer

import FRP.FrABS

import Text.Printf
import System.IO

winSize = (800, 800)
winTitle = "Policy Effects Network (2D Rendering)"
frequency = 0

updateStrat = Parallel
shuffleAgents = False

rngSeed = 42

agentCount = 100 :: Int

initialWealth :: Double
initialWealth = 100

dt :: Double
dt = 1.0

t :: Double
t = 500

completeNetwork = Complete agentCount
erdosRenyiNetwork = ErdosRenyi agentCount 0.2
barbasiAlbertNetwork = BarbasiAlbert barbasiAlbertM0 barbasiAlbertM agentCount
barbasiAlbertM0 = 3
barbasiAlbertM = 1

network = Random erdosRenyiNetwork

replCfg = ReplicationConfig {
    replCfgCount = 4,
    replCfgAgentReplicator = defaultAgentReplicator,
    replCfgEnvReplicator = policyEffectsEnvReplicator network
}

-- TODO: repair

runPolicyEffectsWithRendering :: IO ()
runPolicyEffectsWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects initialWealth network
        
        simulateAndRender initAdefs
                            initEnv
                            params
                            dt
                            frequency
                            winTitle
                            winSize
                            renderPolicyEffectsFrame
                            Nothing

runPolicyEffectsStepsAndWriteToFile :: IO ()
runPolicyEffectsStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects initialWealth network

        let asenv = simulateTime initAdefs initEnv params dt t
        let dynamics = map (\(_, as, _) -> calculateDynamics as) asenv
        let dynamicsFileName = "policyEffectsDynamics_" 
                                ++ show agentCount ++ "agents_" 
                                ++ show t ++ "time.m" 

        let (_, finalAgents, _) = last asenv
        let histFileName = "policyEffectsHistogram_" 
                                ++ show agentCount ++ "agents_" 
                                ++ show t ++ "time.m" 

        writeDynamicsFile dynamicsFileName t 0 dynamics
        writeHistogramFile histFileName t 0 finalAgents

runPolicyEffectsReplicationsAndWriteToFile :: IO ()
runPolicyEffectsReplicationsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- createPolicyEffects initialWealth network

        let assenv = runReplications initAdefs initEnv params dt t replCfg
        let replicationDynamics = map calculateSingleReplicationDynamic assenv
        let dynamics = dynamicsReplMean replicationDynamics

        let fileName = "policyEffectsDynamics_" 
                        ++ show agentCount ++ "agents_" 
                        ++ show t ++ "time_" 
                        ++ show (replCfgCount replCfg) ++ "replications.m"

        writeDynamicsFile fileName t (replCfgCount replCfg) dynamics


calculateDynamics :: [PolicyEffectsAgentObservable] -> (Double, Double, Double)
calculateDynamics aobs = (minWealth, maxWealth, std)
    where
        n = length aobs
        s = foldr (\ao m -> snd ao + m) 0 aobs

        maxWealth = foldr (\ao m -> if (snd ao > m) then snd ao else m) 0 aobs
        minWealth = foldr (\ao m -> if (snd ao < m) then snd ao else m) (initialWealth * fromIntegral agentCount) aobs

        mean = s / fromIntegral n
        dev = sum $ map (\ao -> (snd ao - mean)^2) aobs
        std = sqrt dev

calculateSingleReplicationDynamic :: [(Time, [PolicyEffectsAgentObservable], PolicyEffectsEnvironment)] 
                                    -> [(Double, Double, Double)]
calculateSingleReplicationDynamic = map (\(_, as, _) -> calculateDynamics as)

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

agentWealthToString :: PolicyEffectsAgentObservable -> String
agentWealthToString ao = 
                printf "%.1f" wealth ++ ";" 
    where
        wealth = snd ao

writeDynamicsFile :: String 
                        -> Double
                        -> Int
                        -> [(Double, Double, Double)] -> IO ()
writeDynamicsFile fileName t replications dynamics =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("time = " ++ show t ++ ";")
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
        hPutStrLn fileHdl ("title ('Policy Effects over " ++ show t ++ " time, " ++  (show replications) ++ " replications');")

        hClose fileHdl

writeHistogramFile :: String 
                        -> Double
                        -> Int
                        -> [PolicyEffectsAgentObservable] -> IO ()
writeHistogramFile fileName t replications aobs =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("time = " ++ show t ++ ";")
        hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")
        
        hPutStrLn fileHdl "agentsWealth = ["
        mapM_ (hPutStrLn fileHdl . agentWealthToString) aobs
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "hist(agentsWealth);"
        
        hClose fileHdl