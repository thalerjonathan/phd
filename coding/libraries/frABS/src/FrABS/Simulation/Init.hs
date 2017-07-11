module FrABS.Simulation.Init (
    initSimulation,
    newAgentId
  ) where

import FrABS.Agent.Agent
import FrABS.Simulation.Simulation
import FrABS.Simulation.Internal

import Control.Monad.Random
import Control.Concurrent.STM.TVar

initSimulation :: UpdateStrategy
                    -> Maybe (EnvironmentCollapsing ec l)
                    -> Bool
                    -> Maybe Int
                    -> IO (SimulationParams ec l)
initSimulation updtStrat collFunc shuffAs rngSeed = 
    do
        initRng rngSeed

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

initRng :: Maybe Int -> IO ()
initRng Nothing = return ()
initRng (Just seed) = setStdGen $ mkStdGen seed