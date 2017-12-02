module FRP.FrABS.Simulation.Common 
  (
    SimulationStepOut

  , runEnv
  , shuffleAgents
  , newAgentIn
  , observableAgents
  ) where

import Data.Maybe

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Definitions
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Random.Pure

type SimulationStepOut s e            = (Time, [AgentObservable s], e)

-- TODO: must become a SF
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

newAgentIn :: AgentIn s m e -> AgentIn s m e
newAgentIn oldIn  = 
  oldIn { 
  aiStart = NoEvent
  , aiMessages = NoEvent
  }

observableAgents :: [AgentId] 
                    -> [AgentOut s m e] 
                    -> [AgentObservable s]
observableAgents ais aos = foldl observableAgents [] (zip ais aos)
  where
    observableAgents :: [AgentObservable s] 
                        -> (AgentId, AgentOut s m e) 
                        -> [AgentObservable s] 
    observableAgents acc (aid, ao) 
        | isJust mayObs = (aid, fromJust mayObs) : acc
        | otherwise = acc
      where
        mayObs = aoState ao