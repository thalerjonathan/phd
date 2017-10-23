{-# LANGUAGE FlexibleInstances                 #-}
{-# LANGUAGE MultiParamTypeClasses             #-}
{-# LANGUAGE ScopedTypeVariables               #-}
module FRP.FrABS.Simulation.Simulation (
    AgentObservableAggregator,
    
    simulateIOInit,
    
    simulateTime,
    simulateAggregateTime,

    simulateDebug,
    simulateDebugInternal
  ) where

import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.SeqIteration
import FRP.FrABS.Simulation.ParIteration
import FRP.FrABS.Agent.Agent

import FRP.Yampa

import FRP.Titan.Debug.Core
import FRP.Titan.Debug.CommTCP

type SimulationStepOut s e = (Time, [AgentObservable s], e)
type AgentObservableAggregator s e a = SimulationStepOut s e -> a

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
simulateIOInit adefs e params iterFunc = reactInit
                                                (return ())
                                                iterFunc
                                                (simulate params adefs e)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- RUN THE SIMULATION FOR A FIXED TIME
-------------------------------------------------------------------------------
simulateTime :: [AgentDef s m e]
                -> e
                -> SimulationParams e
                -> DTime
                -> DTime
                -> [SimulationStepOut s e]
simulateTime adefs e params dt t = obs
    where
        steps = floor $ t / dt
        sts = replicate steps (dt, Nothing)
        obs = embed (simulate params adefs e) ((), sts)
        
simulateAggregateTime :: [AgentDef s m e]
                            -> e
                            -> SimulationParams e
                            -> DTime
                            -> Time
                            -> AgentObservableAggregator s e a
                            -> [a]
simulateAggregateTime adefs e params dt t aggrFun = seq agrs agrs
    where
        steps = floor $ t / dt
        sts = replicate steps (dt, Nothing)
        agrSf = arr aggrFun
        sf = simulate params adefs e >>> agrSf
        agrs = embed sf ((), sts)
----------------------------------------------------------------------------------------------------------------------

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
simulateDebugInternal adefs e params inputFunc renderFunc = 
    do
        bridge <- mkTitanCommTCPBridge

        reactimateControl
            bridge                                   -- Communication channels
            defaultPreferences                       -- Simulation preferences
            ([Pause] :: [Command (FooPred s e)]) --[Pause] -- ([] :: [Command FooPred])     -- Initial command queue
            (return ())                                -- IO a: Initial sensing action
            inputFunc             -- (Bool -> IO (DTime, Maybe a)): Continued sensing action
            renderFunc                               -- (Bool -> b -> IO Bool): Rendering/consumption action
            (simulate params adefs e)                 -- SF a b: Signal Function that defines the program 

data FooPred s e = FooPred deriving (Read, Show)

instance Pred (FooPred s e) () (SimulationStepOut s e) where
    evalPred p dt i o = True
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
simulate :: SimulationParams e
            -> [AgentDef s m e]
            -> e
            -> SF () (SimulationStepOut s e)
simulate params adefs e = sf >>> outToObsSf
    where
        asfs = map adBeh adefs
        idGen = simIdGen params
        ais = createStartingAgentIn adefs idGen
        sf = iterationStrategy params asfs ais e 
        outToObsSf = arr outToObs

        outToObs :: (Time, [AgentOut s m e], e) -> SimulationStepOut s e 
        outToObs (t, os, e) = (t, obs, e)
          where
            obs = map agentOutToObservable os
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
iterationStrategy :: SimulationParams e
                        -> [AgentBehaviour s m e]
                        -> [AgentIn s m e]
                        -> e
                        -> SF () (Time, [AgentOut s m e], e)
iterationStrategy params asfs ais e
    | Sequential == strategy = simulateSeq params asfs ais e
    | otherwise = simulatePar params asfs ais e
    where
        strategy = simStrategy params
----------------------------------------------------------------------------------------------------------------------