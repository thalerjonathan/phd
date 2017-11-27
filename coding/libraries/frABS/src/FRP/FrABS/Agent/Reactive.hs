{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive 
  (
    EventSource
  , MessageSource

  , ReactiveBehaviourIgnoreEnv
  , ReactiveBehaviourReadEnv

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

import Control.Monad.Random
import Data.Maybe

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Network
import FRP.FrABS.Extensions.After
import FRP.FrABS.Extensions.SuperSampling
import FRP.FrABS.Random.Monadic 
import FRP.FrABS.Random.Reactive

type EventSource s m e    = SF (AgentIn s m e, AgentOut s m e, e) (Event ())
type MessageSource s m e  = SF (AgentIn s m e, AgentOut s m e, e) (AgentMessage m)

type ReactiveBehaviourIgnoreEnv s m e   = SF (AgentIn s m e) (AgentOut s m e)
type ReactiveBehaviourReadEnv s m e     = SF (AgentIn s m e, e) (AgentOut s m e)

-------------------------------------------------------------------------------
-- MISC
-------------------------------------------------------------------------------
ignoreEnv :: ReactiveBehaviourIgnoreEnv s m e -> AgentBehaviour s m e 
ignoreEnv f = proc (ain, e) -> do
  ao <- f -< ain
  returnA -< (ao, e)

readEnv :: ReactiveBehaviourReadEnv s m e -> AgentBehaviour s m e 
readEnv f = proc (ain, e) -> do
  ao <- f -< (ain, e)
  returnA -< (ao, e)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------
-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnce :: (AgentOut s m e -> AgentOut s m e) -> SF (AgentOut s m e) (AgentOut s m e)
doOnce f = proc ao -> do
  -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
  -- TODO: why is this not firing the first time?
  aoOnceEvt <- once -< (Event . f) ao
  let ao' = event ao id aoOnceEvt
  returnA -< ao'

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOnceR :: s -> AgentBehaviour s m e -> AgentBehaviour s m e
doOnceR s sf = proc (ain, e) -> do
  doEvt <- once -< (Event ())
  if (isEvent doEvt) then (do
    (aout, e') <- sf -< (ain, e)
    returnA -< (aout, e'))
    else 
      returnA -< (agentOutObs s, e)

doNothing :: AgentBehaviour s m e
doNothing = first $ arr (const agentOut)

doNothingObs :: s -> AgentBehaviour s m e
doNothingObs s = first $ arr (const $ agentOutObs s)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
setAgentStateR :: s -> AgentBehaviour s m e
setAgentStateR s = first $ arr ((setAgentState s) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
updateAgentStateR :: s -> (s -> s) -> AgentBehaviour s m e
updateAgentStateR s f = first $ arr ((updateAgentState f) . (const $ agentOutObs s))

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doRepeatedlyEvery :: Time -> s -> AgentBehaviour s m e -> AgentBehaviour s m e
doRepeatedlyEvery t s sf = proc (ain, e) -> do
  doEvt <- repeatedly t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)

-- TODO: this is rubbish and doesnt make sense because of agentOutObs
doOccasionallyEvery :: RandomGen g => g -> Time -> s -> AgentBehaviour s m e -> AgentBehaviour s m e
doOccasionallyEvery g t s sf = proc (ain, e) -> do
  doEvt <- occasionally g t () -< ()
  if (isEvent doEvt) then (do
    (aout', e') <- sf -< (ain, e)
    returnA -< (aout', e'))
    else 
      returnA -< (agentOutObs s, e)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Messaging
-------------------------------------------------------------------------------
-- TODO: sendMessageRepeatedly
-- TODO: sendMessageAfter
-- TODO: sendMessageOnEvent
-- TODO: sendMessageOnMessageReceived
sendMessageOccasionally :: RandomGen g => g 
                          -> Double
                          -> AgentMessage m
                          -> SF (AgentIn s m e, AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionally g rate msg = sendMessageOccasionallySrc g rate (constMsgSource msg)

sendMessageOccasionallySrc :: RandomGen g => g 
                              -> Double
                              -> MessageSource s m e 
                              -> SF (AgentIn s m e, AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionallySrc g rate msgSrc = proc (ain, ao, e) -> do
    sendEvt <- occasionally g rate () -< ()
    if isEvent sendEvt 
      then (do
        msg <- msgSrc -< (ain, ao, e)
        returnA -< sendMessage msg ao)
      else returnA -< ao

sendMessageOccasionallySS :: RandomGen g => g 
                            -> Double
                            -> Int
                            -> AgentMessage m
                            -> SF (AgentIn s m e, AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionallySS g rate ss msg = sendMessageOccasionallySrcSS g rate ss (constMsgSource msg)

sendMessageOccasionallySrcSS :: RandomGen g => g 
                                -> Double
                                -> Int
                                -> MessageSource s m e 
                                -> SF (AgentIn s m e, AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionallySrcSS g rate ss msgSrc = proc aoe@(_, ao, _) -> do
    sendEvtsSS <- superSampling ss (occasionally g rate ()) -< ()
    msgSS <- superSampling ss msgSrc -< aoe
    let ao' = foldr sendMessageOccasionallySrcSSAux ao (zip sendEvtsSS msgSS)
    returnA -< ao'
  where
    sendMessageOccasionallySrcSSAux :: (Event (), AgentMessage m)
                                      -> AgentOut s m e 
                                      -> AgentOut s m e 
    sendMessageOccasionallySrcSSAux (NoEvent, _) ao = ao
    sendMessageOccasionallySrcSSAux (Event (), msg) ao = sendMessage msg ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constMsgReceiverSource :: m -> AgentId -> MessageSource s m e
constMsgReceiverSource m receiver = arr (const (receiver, m))

constMsgSource :: AgentMessage m -> MessageSource s m e
constMsgSource msg = arr (const msg)

randomNeighbourNodeMsgSource :: RandomGen g => g 
                                -> m 
                                -> MessageSource s m (Network l)
randomNeighbourNodeMsgSource g m = proc (ain, _, e) -> do
    let aid = agentId ain
    randNode <- randomSF g -< randomNeighbourNode aid e
    returnA -< (randNode, m)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: RandomGen g => g 
                                -> (s -> Discrete2dCoord) 
                                -> m 
                                -> Bool 
                                -> MessageSource s m (Discrete2d AgentId)
randomNeighbourCellMsgSource g posFunc m ic = proc (_, ao, e) -> do
    let pos = posFunc $ fromJust $ aoState ao
    randCell <- randomSF g -< randomNeighbourCell pos ic e
    returnA -< (randCell, m)

randomAgentIdMsgSource :: RandomGen g => g 
                          -> m 
                          -> Bool 
                          -> MessageSource s m [AgentId]
randomAgentIdMsgSource g m ignoreSelf = proc aoe@(ain, _, agentIds) -> do
    let aid = agentId ain
    randAid <- drawRandomElemSF g -< agentIds
    if True == ignoreSelf && aid == randAid
      then randomAgentIdMsgSource (snd $ split g) m ignoreSelf -< aoe
      else returnA -< (randAid, m)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------
transitionAfter :: Double
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfter t from to = switch (transitionAfterAux t from) (const to)
  where
    transitionAfterAux :: Double 
                            -> AgentBehaviour s m e 
                            -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionAfterAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvent <- after t () -< ()
      returnA -< (aoe, timeoutEvent)

transitionAfterExpSS :: RandomGen g => g 
                    -> Double
                    -> Int
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfterExpSS g t ss from to = switch (transitionAfterExpSSAux t from) (const to)
  where
    transitionAfterExpSSAux :: Double 
                            -> AgentBehaviour s m e 
                            -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionAfterExpSSAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvents <- superSampling ss (afterExp g t ()) -< ()
      let hasEvent = any isEvent timeoutEvents
      let timeoutOccurred = if hasEvent then Event () else NoEvent
      returnA -< (aoe, timeoutOccurred)

transitionAfterExp :: RandomGen g => g 
                    -> Double
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfterExp g t from to = switch (transitionAfterExpAux t from) (const to)
  where
    transitionAfterExpAux :: Double 
                            -> AgentBehaviour s m e 
                            -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionAfterExpAux t from = proc aie -> do
      aoe <- from -< aie
      timeoutEvent <- afterExp g t () -< ()
      returnA -< (aoe, timeoutEvent)

transitionWithUniProb :: RandomGen g => g 
                          -> Double
                          -> AgentBehaviour s m e
                          -> AgentBehaviour s m e
                          -> AgentBehaviour s m e
transitionWithUniProb g p from to = switch (transitionWithUniProbAux from)(const to)
  where
    transitionWithUniProbAux :: AgentBehaviour s m e
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionWithUniProbAux from = proc aie -> do
      aie' <- from -< aie
      evtFlag <- randomSF g -< randomBoolM p
      evt <- iEdge False -< evtFlag
      returnA -< (aie', evt)

transitionWithExpProb :: RandomGen g => g 
                        -> Double
                        -> Double
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
transitionWithExpProb g lambda p from to = switch (transitionWithExpProbAux from) (const to)
  where
    transitionWithExpProbAux :: AgentBehaviour s m e
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionWithExpProbAux from = proc aie -> do
      aie' <- from -< aie
      r <- randomSF g -< randomExpM lambda
      evt <- iEdge False -< (p >= r)
      returnA -< (aie', evt)

transitionOnEvent :: EventSource s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (const to)
  where
    transitionEventAux :: EventSource s m e
                            -> AgentBehaviour s m e
                            -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionEventAux evtSrc from = proc aie@(ain, _) -> do
      (ao, e) <- from -< aie
      evt <- evtSrc -< (ain, ao, e)
      returnA -< ((ao, e), evt)

-- NOTE: assumes state isJust
transitionOnBoolState :: (s -> Bool)
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionOnBoolState boolStateFunc from to = switch (transitionOnBoolStateAux boolStateFunc from) (const to)
  where
    transitionOnBoolStateAux :: (s -> Bool)
                                -> AgentBehaviour s m e
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionOnBoolStateAux boolStateFunc from = proc aie -> do
      (ao, e) <- from -< aie
      let state = fromJust $ aoState ao
      let evtFlag = boolStateFunc state
      evt <- iEdge False -< evtFlag
      returnA -< ((ao, e), evt)

transitionOnMessage :: (Eq m) => m 
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
transitionOnMessage msg from to = transitionOnEvent (messageEventSource msg) from to

transitionOnEventWithGuard :: RandomGen g => g 
                            -> EventSource s m e
                            -> Rand g Bool
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionOnEventWithGuard g evtSrc guardAction from to = 
    switch (transitionEventWithGuardAux evtSrc from) (const to)
  where
    transitionEventWithGuardAux :: EventSource s m e
                                    -> AgentBehaviour s m e 
                                    -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
    transitionEventWithGuardAux evtSrc from = proc aie@(ain, _) -> do
      (ao, e) <- from -< aie
      evt <- evtSrc -< (ain, ao, e)
      flag <- randomSF g -< guardAction
      if (isEvent evt && flag)
        then returnA -< ((ao, e), Event())
        else returnA -< ((ao, e), NoEvent)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- EVENT-Sources
-------------------------------------------------------------------------------
-- TODO: how can we formulate this in point-free arrow style?
messageEventSource :: (Eq m) => m -> EventSource s m e
messageEventSource msg = proc (ain, _, _) -> do
  evt <- iEdge False -< hasMessage msg ain
  returnA -< evt
-------------------------------------------------------------------------------