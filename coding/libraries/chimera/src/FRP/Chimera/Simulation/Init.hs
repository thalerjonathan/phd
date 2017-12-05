module FRP.Chimera.Simulation.Init 
  (
    SimulationParams (..)
  , UpdateStrategy (..)

  , initRng
  , initSimulation
  , initSimNoEnv
  , newAgentId
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.Random

import FRP.Chimera.Agent.Interface
import FRP.Chimera.Environment.Definitions
import FRP.Chimera.Simulation.Internal

data UpdateStrategy      = Sequential | Parallel deriving (Eq, Show)

data SimulationParams e = SimulationParams 
  {
    simStrategy       :: UpdateStrategy
  , simEnvBehaviour   :: Maybe (EnvironmentBehaviour e)
  , simEnvFold        :: Maybe (EnvironmentFolding e)
  , simShuffleAgents  :: Bool
  , simRng            :: StdGen
  , simIdGen          :: TVar Int
  }

initSimulation :: UpdateStrategy
                  -> Maybe (EnvironmentBehaviour e)
                  -> Maybe (EnvironmentFolding e)
                  -> Bool
                  -> Maybe Int
                  -> IO (SimulationParams e)
initSimulation updtStrat beh foldEnvFun shuffAs rngSeed = do
  initRng rngSeed

  rng <- getSplit
  agentIdVar <- newTVarIO 0

  return SimulationParams {
      simStrategy = updtStrat
    , simEnvBehaviour = beh
    , simEnvFold = foldEnvFun
    , simShuffleAgents = shuffAs
    , simRng = rng
    , simIdGen = agentIdVar
    }

initSimNoEnv :: UpdateStrategy
                -> Bool
                -> Maybe Int
                -> IO (SimulationParams e)
initSimNoEnv updtStrat shuffAs rngSeed = initSimulation updtStrat Nothing Nothing shuffAs rngSeed

newAgentId :: SimulationParams e -> AgentId
newAgentId SimulationParams { simIdGen = idGen } = incrementAtomicallyUnsafe idGen

initRng :: Maybe Int -> IO ()
initRng Nothing = return ()
initRng (Just seed) = setStdGen $ mkStdGen seed