module FRP.FrABS.Simulation.Common 
  (
    SimulationStepOut

  , runEnv
  , shuffleAgents
  , newAgentIn
  ) where

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Definitions
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Utils

type SimulationStepOut s e            = (Time, [AgentObservable s], e)

runEnv :: DTime -> SimulationParams e -> e -> (e, SimulationParams e)
runEnv dt params e = maybe (e, params) (runEnvAux params e) mayEnvBeh
  where
    mayEnvBeh = simEnvBehaviour params

    runEnvAux :: SimulationParams e -> e -> EnvironmentBehaviour e -> (e, SimulationParams e)
    runEnvAux params e envBeh = (e', params')
      where
        (envBeh', e') = runAndFreezeSF envBeh e dt
        params' = params { simEnvBehaviour = Just envBeh' }

shuffleAgents :: SimulationParams e 
                -> [a] 
                -> [b] 
                -> (SimulationParams e, [a], [b])
shuffleAgents params as bs 
    | doShuffle = (params', as', bs')
    | otherwise = (params, as, bs)
  where
    doShuffle = simShuffleAgents params
    g = simRng params 

    sfsIns = zip as bs
    (shuffledSfsIns, g') = fisherYatesShuffle g sfsIns

    params' = params { simRng = g' }
    (as', bs') = unzip shuffledSfsIns

newAgentIn :: AgentIn s m e
                -> AgentOut s m e
                -> AgentIn s m e
newAgentIn oldIn newOut = newIn
  where
    newIn = oldIn { aiStart = NoEvent
                  -- aiState = aoState newOut
                  , aiMessages = NoEvent
                  , aiRng = aoRng newOut -- TODO: want to get rid of this
                  }