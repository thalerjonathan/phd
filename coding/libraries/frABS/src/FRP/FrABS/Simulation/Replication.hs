module FRP.FrABS.Simulation.Replication 
    (
      AgentDefReplicator
    , EnvironmentReplicator
    , Replication
    
    , ReplicationConfig (..)

    , defaultEnvReplicator
    , defaultAgentReplicator

    , runReplications
    , runReplicationsWithAggregation
    ) where

import Control.Monad.Random
import Control.Parallel.Strategies

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Init

type AgentDefReplicator s m e = (StdGen -> AgentDef s m e -> (AgentDef s m e, StdGen))
type EnvironmentReplicator e = (StdGen -> e -> (e, StdGen))
type Replication s e = [SimulationStepOut s e]
type ReplicationAggregate a = [a]

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

runReplicationsWithAggregation :: [AgentDef s m e]
                                -> e
                                -> SimulationParams e
                                -> DTime
                                -> Time
                                -> ReplicationConfig s m e
                                -> AgentObservableAggregator s e a
                                -> [ReplicationAggregate a]
runReplicationsWithAggregation ads e params dt t replCfg aggrFunc = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsWithAggregationAux ads e params aggrFunc) replRngs

    runReplicationsWithAggregationAux :: [AgentDef s m e]
                                        -> e
                                        -> SimulationParams e
                                        -> AgentObservableAggregator s e a
                                        -> StdGen 
                                        -> ReplicationAggregate a
    runReplicationsWithAggregationAux ads e params aggrFunc replRng = 
        simulateAggregateTime ads' e (params { simRng = replRng' }) dt t aggrFunc
      where
        (ads', replRng') = foldr adsFoldAux ([], replRng) ads

runReplications :: [AgentDef s m e]
                    -> e
                    -> SimulationParams e
                    -> DTime
                    -> Time
                    -> ReplicationConfig s m e
                    -> [Replication s e]
runReplications ads e params dt t replCfg = result
  where
    replCount = replCfgCount replCfg
    (replRngs, _) = duplicateRng replCount (simRng params)

    -- NOTE: replace by rseq if no hardware-parallelism should be used
    result = parMap rpar (runReplicationsAux ads e params) replRngs

    runReplicationsAux :: [AgentDef s m e]
                            -> e
                            -> SimulationParams e
                            -> StdGen 
                            -> Replication s e
    runReplicationsAux ads e params replRng = simulateTime ads' e (params { simRng = replRng' }) dt t
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