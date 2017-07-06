module FrABS.Simulation.SimulationUtils (
	initRng,
	initSimParams,

    runReplications,
    newAgentId
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation
import FrABS.Simulation.Utils

import Control.Monad.Random
import Control.Parallel.Strategies
import Control.Concurrent.STM.TVar

import Debug.Trace

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g

initSimParams :: UpdateStrategy
				-> Maybe (EnvironmentCollapsing ec l)
				-> Bool
				-> IO (SimulationParams ec l)
initSimParams updtStrat collFunc shuffAs = 
    do
        rng <- getSplit
        agentIdVar <- newTVarIO 0

        return SimulationParams {
            simStrategy = updtStrat,
            simEnvCollapse = collFunc,
            simShuffleAgents = shuffAs,
            simRng = rng,
            simIdGen = agentIdVar
        }

newAgentId :: SimulationParams ec l -> AgentId
newAgentId SimulationParams { simIdGen = idGen } = incrementAtomicallyUnsafe idGen

runReplications :: [AgentDef s m ec l]
                    -> Environment ec l
                    -> SimulationParams ec l
                    -> Double
                    -> Int
                    -> Int
                    -> [[([AgentOut s m ec l], Environment ec l)]]
runReplications ads env params dt steps replCount = 
        parMap rpar (runReplicationsAux ads env params) replRngs -- NOTE: replace by rseq if no hardware-parallelism should be used
    where
        (replRngs, _) = duplicateRng replCount (simRng params)
        
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