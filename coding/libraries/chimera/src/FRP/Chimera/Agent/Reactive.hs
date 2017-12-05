{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Agent.Reactive 
  (
    EventSource
  , DataSource

  , AgentIgnoreEnv
  , AgentReadEnv

  , ignoreEnv
  , readEnv

  , doOnce
  , doOnceR
  , doNothing
  , doNothingObs
  , doRepeatedlyEvery
  , doOccasionallyEvery

  , setAgentStateR
  , updateAgentStateR

  , sendMessageOccasionallySrc
  , sendMessageOccasionally

  , sendMessageOccasionallySrcSS
  , sendMessageOccasionallySS

  , constMsgReceiverSource
  , constMsgSource
  , randomNeighbourNodeMsgSource
  , randomNeighbourCellMsgSource
  , randomAgentIdMsgSource

  , transitionAfter
  , transitionAfterExp
  , transitionAfterExpSS
  , transitionWithUniProb
  , transitionWithExpProb
  , transitionOnEvent
  , transitionOnMessage
  , transitionOnEventWithGuard
  , transitionOnBoolState

  , messageEventSource
  ) where

import Data.Maybe

import Control.Monad.State
import Control.Monad.Random
import FRP.BearRiver

import FRP.Chimera.Agent.Agent
import FRP.Chimera.Agent.Monad
import FRP.Chimera.Environment.Discrete
import FRP.Chimera.Environment.Network
import FRP.Chimera.Random.Monadic 
import FRP.Chimera.Random.Reactive

type EventSource m o d e    = SF m (AgentIn o d e, e) (Event ())
type DataSource m o d e     = SF m (AgentIn o d e, e) (AgentData d)

type AgentIgnoreEnv m o d e = SF (StateT (AgentOut m o d e) m) (AgentIn o d e) ()
type AgentReadEnv m o d e   = SF (StateT (AgentOut m o d e) m) (AgentIn o d e, e) () 

-------------------------------------------------------------------------------
-- MISC
-------------------------------------------------------------------------------
ignoreEnv :: Monad m => AgentIgnoreEnv m o d e -> Agent m o d e 
ignoreEnv f = proc (ain, e) -> do
  _ <- f -< ain
  returnA -< e

readEnv :: Monad m => AgentReadEnv m o d e -> Agent m o d e
readEnv f = proc (ain, e) -> do
  _ <- f -< (ain, e)
  returnA -< e
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------
{-
-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnce :: MonadState (AgentOut m o d e) m
       => (AgentOut m o d e -> AgentOut m o d e) 
       -> Agent m o d e
doOnce f = proc ao -> do
  -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
  -- TODO: why is this not firing the first time?
  aoOnceEvt <- once -< (Event . f) ao
  let ao' = event ao id aoOnceEvt
  returnA -< ao'

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnceR :: s -> Agent m o d e -> Agent m o d e
doOnceR s sf = proc (ain, e) -> do
  doEvt <- once -< (Event ())
  if (isEvent doEvt) then (do
    (aout, e') <- sf -< (ain, e)
    returnA -< (aout, e'))
    else 
      returnA -< (agentOutObs s, e)

doNothing :: Agent m o d e
doNothing = first $ arr (const agentOut)

doNothingObs :: s -> Agent m o d e
doNothingObs s = first $ arr (const $ agentOutObs s)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
setAgentStateR :: s -> Agent m o d e
setAgentStateR s = first $ arr ((setAgentState s) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
updateAgentStateR :: s -> (s -> s) -> Agent m o d e
updateAgentStateR s f = first $ arr ((updateAgentState f) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doRepeatedlyEvery :: Time -> s -> Agent m o d e -> Agent m o d e
doRepeatedlyEvery t s sf = proc (ain, e) -> do
  doEvt <- repeatedly t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOccasionallyEvery :: RandomGen g => g -> Time -> s -> Agent m o d e -> Agent m o d e
doOccasionallyEvery g t s sf = proc (ain, e) -> do
  doEvt <- occasionally g t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)
      -}
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGING / DATA-FLOWS
-------------------------------------------------------------------------------
dataFlowS :: MonadState (AgentOut m o d e) m
          => SF m (AgentData d) ()
dataFlowS = proc d -> do
  _ <- arrM (\d -> dataFlowM d) -< d
  returnA -< ()

-- TODO: sendMessageRepeatedly
-- TODO: sendMessageAfter
-- TODO: sendMessageOnEvent
-- TODO: sendMessageOnMessageReceived
dataFlowOccasionally :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                     => Double
                     -> AgentData d
                     -> SF m (AgentIn o d e, e) ()
dataFlowOccasionally rate d = dataFlowOccasionallySrc rate (constDataSource d)

dataFlowOccasionallySrc :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                        => Double
                        -> DataSource m o d e 
                        -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySrc rate dfSrc = proc (ain, e) -> do
  sendEvt <- occasionally rate () -< ()
  if isEvent sendEvt 
    then (do
      d <- dfSrc      -< (ain, e)
      _ <- dataFlowS  -< d
      returnA         -< ())
    else returnA      -< ()

dataFlowOccasionallySS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                       => Double
                       -> Int
                       -> AgentData d
                       -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySS rate ss d = dataFlowOccasionallySrcSS rate ss (constDataSource d)

dataFlowOccasionallySrcSS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                          => Double
                          -> Int
                          -> DataSource m o d e 
                          -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySrcSS rate ss dfSrc = proc aie -> do
    sendEvtsSS <- superSamplingUniform ss (occasionally rate ())  -< ()
    dfSS       <- superSamplingUniform ss dfSrc                   -< aie
    _          <- arrM $ mapM dataFlowOccasionallySrcSSAux        -< (zip sendEvtsSS dfSS)
    returnA -< ()
  where
    dataFlowOccasionallySrcSSAux :: MonadState (AgentOut m o d e) m
                                 => (Event (), AgentData d)
                                 -> m ()
    dataFlowOccasionallySrcSSAux (NoEvent, _) = return ()
    dataFlowOccasionallySrcSSAux (Event (), d) = dataFlowM d
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constDataReceiverSource :: Monad m => d -> AgentId -> DataSource m o d e
constDataReceiverSource d receiver = constant (receiver, d)

constDataSource :: Monad m => AgentData d -> DataSource m o d e
constDataSource = constant

--type DataSource m o d e     = SF m (AgentIn o d e, e) (AgentData d)

randomNeighbourNodeMsgSource :: MonadRandom m
                             => d
                             -> DataSource m o d (Network l)
randomNeighbourNodeMsgSource d = proc (ain, e) -> do
  let aid = agentId ain
  randNode <- arrM (\(aid, e) -> randomNeighbourNode aid e) -< (aid, e)
  returnA -< (randNode, d)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: MonadRandom m
                             => (o -> Discrete2dCoord) 
                             -> d 
                             -> Bool 
                             -> DataSource m o d (Discrete2d AgentId)
randomNeighbourCellMsgSource posFunc d ic = proc (_, e) -> do
  let pos = posFunc $ fromJust $ agentObservable
  randCell <- arrM (\(pos, e) -> randomNeighbourCell pos ic e) -< (pos, e)
  returnA -< (randCell, d)

randomAgentIdMsgSource :: MonadRandom m
                       => d
                       -> Bool 
                       -> DataSource m o d [AgentId]
randomAgentIdMsgSource d ignoreSelf = proc aie@(ain, agentIds) -> do
  let aid = agentId ain
  randAid <- drawRandomElemSF g -< agentIds
  if True == ignoreSelf && aid == randAid
    then randomAgentIdMsgSource (snd $ split g) m ignoreSelf -< aie
    else returnA -< (randAid, d)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------
transitionAfter :: Double
                    -> Agent m o d e
                    -> Agent m o d e
                    -> Agent m o d e
transitionAfter t from to = switch (transitionAfterAux t from) (const to)
  where
    transitionAfterAux :: Double 
                            -> Agent m o d e 
                            -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionAfterAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvent <- after t () -< ()
      returnA -< (aoe, timeoutEvent)

transitionAfterExpSS :: RandomGen g => g 
                    -> Double
                    -> Int
                    -> Agent m o d e
                    -> Agent m o d e
                    -> Agent m o d e
transitionAfterExpSS g t ss from to = switch (transitionAfterExpSSAux t from) (const to)
  where
    transitionAfterExpSSAux :: Double 
                            -> Agent m o d e 
                            -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionAfterExpSSAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvents <- superSampling ss (afterExp g t ()) -< ()
      let hasEvent = any isEvent timeoutEvents
      let timeoutOccurred = if hasEvent then Event () else NoEvent
      returnA -< (aoe, timeoutOccurred)

transitionAfterExp :: RandomGen g => g 
                    -> Double
                    -> Agent m o d e
                    -> Agent m o d e
                    -> Agent m o d e
transitionAfterExp g t from to = switch (transitionAfterExpAux t from) (const to)
  where
    transitionAfterExpAux :: Double 
                            -> Agent m o d e 
                            -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionAfterExpAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvent <- afterExp g t () -< ()
      returnA -< (aoe, timeoutEvent)

transitionWithUniProb :: RandomGen g => g 
                          -> Double
                          -> Agent m o d e
                          -> Agent m o d e
                          -> Agent m o d e
transitionWithUniProb g p from to = switch (transitionWithUniProbAux from)(const to)
  where
    transitionWithUniProbAux :: Agent m o d e
                                -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionWithUniProbAux from = proc aie -> do
      aie' <- from -< aie
      evtFlag <- randomSF g -< randomBoolM p
      evt <- iEdge False -< evtFlag
      returnA -< (aie', evt)

transitionWithExpProb :: RandomGen g => g 
                        -> Double
                        -> Double
                        -> Agent m o d e
                        -> Agent m o d e
                        -> Agent m o d e
transitionWithExpProb g lambda p from to = switch (transitionWithExpProbAux from) (const to)
  where
    transitionWithExpProbAux :: Agent m o d e
                                -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionWithExpProbAux from = proc aie -> do
      aie' <- from -< aie
      r <- randomSF g -< randomExpM lambda
      evt <- iEdge False -< (p >= r)
      returnA -< (aie', evt)

transitionOnEvent :: EventSource m o d e
                    -> Agent m o d e
                    -> Agent m o d e
                    -> Agent m o d e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (const to)
  where
    transitionEventAux :: EventSource m o d e
                            -> Agent m o d e
                            -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionEventAux evtSrc from = proc aie@(ain, _) -> do
      (ao, e) <- from -< aie
      evt <- evtSrc -< (ain, ao, e)
      returnA -< ((ao, e), evt)

-- NOTE: assumes state isJust
transitionOnBoolState :: (s -> Bool)
                            -> Agent m o d e
                            -> Agent m o d e
                            -> Agent m o d e
transitionOnBoolState boolStateFunc from to = switch (transitionOnBoolStateAux boolStateFunc from) (const to)
  where
    transitionOnBoolStateAux :: (s -> Bool)
                                -> Agent m o d e
                                -> SF (AgentIn o d e, e) ((AgentOut m o d e, e), Event ())
    transitionOnBoolStateAux boolStateFunc from = proc aie -> do
      (ao, e) <- from -< aie
      let state = fromJust $ aoState ao
      let evtFlag = boolStateFunc state
      evt <- iEdge False -< evtFlag
      returnA -< ((ao, e), evt)

transitionOnData :: Eq d => d 
                 -> Agent m o d e
                 -> Agent m o d e
                 -> Agent m o d e
transitionOnData d from to = transitionOnEvent (dataEventSource d) from to

transitionOnEventWithGuard :: (MonadRandom m, RandomGen g)
                           => EventSource m o d e
                           -> Rand g Bool
                           -> Agent m o d e
                           -> Agent m o d e
                           -> Agent m o d e
transitionOnEventWithGuard evtSrc guardAction from to = 
    switch (transitionEventWithGuardAux evtSrc from) (const to)
  where
    transitionEventWithGuardAux :: MonadRandom m
                                => EventSource m o d e
                                -> Agent m o d e 
                                -> SF m (AgentIn o d e, e) (e, Event ())
    transitionEventWithGuardAux evtSrc from = proc aie@(ain, _) -> do
      e <- from -< aie
      evt <- evtSrc -< (ain, e)
      flag <- arrM_ (lift $ guardAction) -< ()
      returnA -< if (isEvent evt && flag)
                   then (e, Event())
                   else (e, NoEvent)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- EVENT-Sources
-------------------------------------------------------------------------------
dataEventSource :: (Eq d, Monad m) => d -> EventSource m o d e
dataEventSource d = proc (ain, _) -> do
  evt <- edgeFrom False -< hasDataFlow d ain
  returnA -< evt
-------------------------------------------------------------------------------