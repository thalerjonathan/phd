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

type AgentObservableAggregator s e a = (([AgentObservable s], e) -> a) 

-------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
-------------------------------------------------------------------------------
simulateIOInit :: [AgentDef s m e]
                    -> e
                    -> SimulationParams e
                    -> (ReactHandle () ([AgentObservable s], e)
                            -> Bool
                            -> ([AgentObservable s], e)
                            -> IO Bool)
                    -> IO (ReactHandle () ([AgentObservable s], e))
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
                -> [([AgentObservable s], e)]
simulateTime adefs e params dt t = embed (simulate params adefs e) ((), sts)
    where
        steps = floor $ t / dt
        sts = replicate steps (dt, Nothing)

simulateAggregateTime :: [AgentDef s m e]
                            -> e
                            -> SimulationParams e
                            -> DTime
                            -> DTime
                            -> AgentObservableAggregator s e a
                            -> [a]
simulateAggregateTime adefs e params dt t aggrFun = agrs
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
                -> (Bool -> ([AgentObservable s], e) -> IO Bool)
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
                        -> (Bool -> ([AgentObservable s], e) -> IO Bool)
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

instance Pred (FooPred s e) () ([AgentObservable s], e) where
    evalPred p dt i o = True
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
simulate :: SimulationParams e
            -> [AgentDef s m e]
            -> e
            -> SF () ([AgentObservable s], e)
simulate params adefs e = sf >>> outToObs'
    where
        asfs = map adBeh adefs
        idGen = simIdGen params
        ais = createStartingAgentIn adefs idGen
        sf = iterationStrategy params asfs ais e 
        outToObs' = first (arr $ map agentOutToObservable)
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
iterationStrategy :: SimulationParams e
                        -> [AgentBehaviour s m e]
                        -> [AgentIn s m e]
                        -> e
                        -> SF () ([AgentOut s m e], e)
iterationStrategy params asfs ais e
    | Sequential == strategy = simulateSeq params asfs ais e
    | Parallel == strategy = simulatePar params asfs ais e
    where
        strategy = simStrategy params
----------------------------------------------------------------------------------------------------------------------