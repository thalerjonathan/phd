{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE ScopedTypeVariables               #-}
module FRP.Chimera.Simulation.Simulation 
  (
    AgentObservableAggregator

  , simulateIOInit

  , simulateTime
  , simulateTimeDeltas
  , simulateAggregateTimeDeltas
  , simulateAggregateTime

  , simulateDebug
  , simulateDebugInternal
  ) where

import FRP.BearRiver

import FRP.Chimera.Agent.Agent
import FRP.Chimera.Simulation.Common
import FRP.Chimera.Simulation.Init
import FRP.Chimera.Simulation.ParIteration
import FRP.Chimera.Simulation.SeqIteration

type AgentObservableAggregator s e a  = SimulationStepOut s e -> a

-------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
-------------------------------------------------------------------------------
simulateIOInit :: [AgentDef s m e]
                  -> e
                  -> SimulationParams e
                  -> (ReactHandle () (SimulationStepOut s e)
                          -> Bool
                          -> SimulationStepOut s e
                          -> IO Bool)
                  -> IO (ReactHandle () (SimulationStepOut s e))
simulateIOInit adefs e params iterFunc = reactInit (return ()) iterFunc (simulate params adefs e)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RUN THE SIMULATION FOR A FIXED TIME
-------------------------------------------------------------------------------
simulateTime :: [AgentDef s m e]
                -> e
                -> SimulationParams e
                -> DTime
                -> Time
                -> [SimulationStepOut s e]
simulateTime adefs e params dt t = embed (simulate params adefs e) ((), sts)
  where
    steps = floor $ t / dt
    sts = replicate steps (dt, Nothing)

simulateTimeDeltas :: [AgentDef s m e]
                      -> e
                      -> SimulationParams e
                      -> [DTime]
                      -> [SimulationStepOut s e]
simulateTimeDeltas adefs e params dts = embed (simulate params adefs e) ((), sts)
  where
    sts = zip dts (repeat Nothing) 

simulateAggregateTimeDeltas :: [AgentDef s m e]
                                -> e
                                -> SimulationParams e
                                -> [DTime]
                                -> AgentObservableAggregator s e a
                                -> [a]
simulateAggregateTimeDeltas adefs e params dts aggrFun = seq agrs agrs -- optimization
  where
    sts = zip dts (repeat Nothing) 
    agrSf = arr aggrFun
    sf = simulate params adefs e >>> agrSf
    agrs = embed sf ((), sts)

simulateAggregateTime :: [AgentDef s m e]
                          -> e
                          -> SimulationParams e
                          -> DTime
                          -> Time
                          -> AgentObservableAggregator s e a
                          -> [a]
simulateAggregateTime adefs e params dt t aggrFun = seq agrs agrs -- optimization
  where
    steps = floor $ t / dt
    sts = replicate steps (dt, Nothing)
    agrSf = arr aggrFun
    sf = simulate params adefs e >>> agrSf
    agrs = embed sf ((), sts)
----------------------------------------------------------------------------------------------------------------------

{-
------------------------------------------------------------------------------------------------------------------------
-- DEBUGGING THE SIMULATION USING HASKELL-TITAN
------------------------------------------------------------------------------------------------------------------------
simulateDebug :: forall s e m .
                (Show s, Read s, Show e, Read e)
                => [AgentDef s m e]
                -> e
                -> SimulationParams e
                -> Double
                -> (Bool -> SimulationStepOut s e -> IO Bool)
                -> IO ()
simulateDebug adefs e params dt renderFunc = 
  simulateDebugInternal
    adefs
    e
    params
    (\_ -> return (dt, Nothing))
    renderFunc

simulateDebugInternal :: forall s e m .
                        (Show s, Read s, Show e, Read e)
                        => [AgentDef s m e]
                        -> e
                        -> SimulationParams e
                        -> (Bool -> IO (DTime, Maybe ()))
                        -> (Bool -> SimulationStepOut s e -> IO Bool)
                        -> IO ()
simulateDebugInternal adefs e params inputFunc renderFunc = do
  bridge <- mkTitanCommTCPBridge

  reactimateControl
      bridge                                    -- Communication channels
      defaultPreferences                        -- Simulation preferences
      ([Pause] :: [Command (FooPred s e)])      --[Pause] -- ([] :: [Command FooPred])     -- Initial command queue
      (return ())                               -- IO a: Initial sensing action
      inputFunc                                 -- (Bool -> IO (DTime, Maybe a)): Continued sensing action
      renderFunc                                -- (Bool -> b -> IO Bool): Rendering/consumption action
      (simulate params adefs e)                 -- SF a b: Signal Function that defines the program 

data FooPred s e = FooPred deriving (Read, Show)

instance Pred (FooPred s e) () (SimulationStepOut s e) where
    evalPred _ _ _ _ = True
    -}
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
simulate :: SimulationParams e
            -> [AgentDef s m e]
            -> e
            -> SF () (SimulationStepOut s e)
simulate params adefs e = sf
  where
    asfs = map adBeh adefs
    idGen = simIdGen params
    ais = startingAgentIn adefs idGen
    sf = iterationStrategy params asfs ais e 
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
iterationStrategy :: SimulationParams e
                      -> [AgentBehaviour s m e]
                      -> [AgentIn s m e]
                      -> e
                      -> SF () (SimulationStepOut s e)
iterationStrategy params asfs ais e
    | Sequential == strategy = simulateSeq params asfs ais e
    | otherwise = simulatePar params asfs ais e
  where
    strategy = simStrategy params
----------------------------------------------------------------------------------------------------------------------