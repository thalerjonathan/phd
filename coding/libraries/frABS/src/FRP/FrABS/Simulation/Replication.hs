module FRP.FrABS.Simulation.Replication (
    AgentDefReplicator,
    EnvironmentReplicator,

    ReplicationConfig (..),

    defaultEnvReplicator,
    defaultAgentReplicator,

    runReplications
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Env.Environment
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Internal

import Control.Monad.Random
import Control.Parallel.Strategies
import Control.Concurrent.STM.TVar

type AgentDefReplicator s m ec l = (StdGen -> AgentDef s m ec l -> (AgentDef s m ec l, StdGen))
type EnvironmentReplicator ec l = (StdGen -> Environment ec l -> (Environment ec l, StdGen))

data ReplicationConfig s m ec l = ReplicationConfig {
    replCfgCount :: Int,
    replCfgAgentReplicator :: AgentDefReplicator s m ec l,
    replCfgEnvReplicator :: EnvironmentReplicator ec l
}

defaultEnvReplicator :: EnvironmentReplicator ec l
defaultEnvReplicator rng env = ( env', rng'')
    where
        (rng', rng'') = split rng
        env' = env { envRng = rng' }

defaultAgentReplicator :: AgentDefReplicator s m ec l
defaultAgentReplicator rng adef = (adef', rng'')
    where
        (rng', rng'') = split rng
        adef' = adef { adRng = rng' }

runReplications :: [AgentDef s m ec l]
                    -> Environment ec l
                    -> SimulationParams ec l
                    -> Double
                    -> Int
                    -> ReplicationConfig s m ec l
                    -> [[([AgentOut s m ec l], Environment ec l)]]
runReplications ads env params dt steps replCfg = result
    where
        replCount = replCfgCount replCfg
        (replRngs, _) = duplicateRng replCount (simRng params)

        -- NOTE: replace by rseq if no hardware-parallelism should be used
        result = parMap rpar (runReplicationsAux ads env params) replRngs
        
        runReplicationsAux :: [AgentDef s m ec l]
                                -> Environment ec l
                                -> SimulationParams ec l
                                -> StdGen 
                                -> [([AgentOut s m ec l], Environment ec l)]
        runReplicationsAux ads env params replRng = processSteps ads' env (params { simRng = replRng' }) dt steps
            where
                (ads', replRng') = foldr adsFoldAux ([], replRng) ads

                adsFoldAux :: AgentDef s m ec l -> ([AgentDef s m ec l], StdGen) -> ([AgentDef s m ec l], StdGen)
                adsFoldAux ad (adsAcc, rngAcc) = (ad' : adsAcc, rng')
                    where
                        (rng, rng') = split rngAcc
                        ad' = ad { adRng = rng }

        duplicateRng :: Int -> StdGen -> ([StdGen], StdGen)
        duplicateRng n g = duplicateRngAux n g []
            where
                duplicateRngAux :: Int -> StdGen -> [StdGen] -> ([StdGen], StdGen)
                duplicateRngAux 0 g acc = (acc, g)
                duplicateRngAux n g acc = duplicateRngAux (n-1) g' (g'' : acc)
                    where
                        (g', g'') = split g