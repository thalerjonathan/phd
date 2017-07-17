module FRP.FrABS.Simulation.Replication (
    AgentDefReplicator,
    EnvironmentReplicator,

    ReplicationConfig (..),

    defaultEnvReplicator,
    defaultAgentReplicator,

    runReplications
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Simulation

import Control.Monad.Random
import Control.Parallel.Strategies

type AgentDefReplicator s m e = (StdGen -> AgentDef s m e -> (AgentDef s m e, StdGen))
type EnvironmentReplicator e = (StdGen -> e -> (e, StdGen))

data ReplicationConfig s m e = ReplicationConfig {
    replCfgCount :: Int,
    replCfgAgentReplicator :: AgentDefReplicator s m e,
    replCfgEnvReplicator :: EnvironmentReplicator e
}

defaultEnvReplicator :: EnvironmentReplicator e
defaultEnvReplicator rng e = (e, rng)

defaultAgentReplicator :: AgentDefReplicator s m e
defaultAgentReplicator rng adef = (adef', rng'')
    where
        (rng', rng'') = split rng
        adef' = adef { adRng = rng' }

runReplications :: [AgentDef s m e]
                    -> e
                    -> SimulationParams e
                    -> Double
                    -> Int
                    -> ReplicationConfig s m e
                    -> [[([AgentOut s m e], e)]]
runReplications ads e params dt steps replCfg = result
    where
        replCount = replCfgCount replCfg
        (replRngs, _) = duplicateRng replCount (simRng params)

        -- NOTE: replace by rseq if no hardware-parallelism should be used
        result = parMap rpar (runReplicationsAux ads e params) replRngs
        
        runReplicationsAux :: [AgentDef s m e]
                                -> e
                                -> SimulationParams e
                                -> StdGen 
                                -> [([AgentOut s m e], e)]
        runReplicationsAux ads e params replRng = processSteps ads' e (params { simRng = replRng' }) dt steps
            where
                (ads', replRng') = foldr adsFoldAux ([], replRng) ads

                adsFoldAux :: AgentDef s m e -> ([AgentDef s m e], StdGen) -> ([AgentDef s m e], StdGen)
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