module FRP.FrABS.Simulation.Init (
    initSimulation,
    newAgentId
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Internal

import Control.Monad.Random
import Control.Concurrent.STM.TVar

initSimulation :: UpdateStrategy
                    -> Maybe (EnvironmentCollapsing e)
                    -> Bool
                    -> Maybe Int
                    -> IO (SimulationParams e)
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

newAgentId :: SimulationParams e -> AgentId
newAgentId SimulationParams { simIdGen = idGen } = incrementAtomicallyUnsafe idGen

initRng :: Maybe Int -> IO ()
initRng Nothing = return ()
initRng (Just seed) = setStdGen $ mkStdGen seed