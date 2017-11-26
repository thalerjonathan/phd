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

  , afterExp
  , superSampling
  ) where

import Control.Monad.Random
import Data.Maybe

import FRP.Yampa
import FRP.Yampa.InternalCore

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Random
import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Network
import FRP.FrABS.Simulation.Internal

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
sendMessageOccasionallySrcSS g rate ss msgSrc = proc aoe -> do
    sendEvts <- superSampling ss (occasionally g rate ()) -< ()
    (_, ao', _) <- foldrSF (sendMessageOccasionallySrcSSAux msgSrc) -< (aoe, sendEvts)
    returnA -< ao'
  where
    sendMessageOccasionallySrcSSAux :: MessageSource s m e  
                                     -> SF (Event (), (AgentIn s m e, AgentOut s m e, e)) (AgentIn s m e, AgentOut s m e, e)
    sendMessageOccasionallySrcSSAux msgSrc = proc (evt, aoe@(ain, ao, e)) -> do
      if isEvent evt
        then (do
          msg <- msgSrc -< aoe
          let ao' = sendMessage msg ao
          returnA -< (ain, ao', e))
        else returnA -< aoe
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
      --let (evtFlag, ao') = agentRandom (randomBoolM p) ao
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
      -- let (r, ao') = agentRandom (randomExpM lambda) ao
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

-------------------------------------------------------------------------------
-- ADDITIONAL SIGNAL-FUNCTIONS
-------------------------------------------------------------------------------
foldrSF :: Foldable t => SF (a, b) b -> SF (b, t a) b
foldrSF sf = SF { sfTF = tf0 } 
  where
    tf0 (acc, as) = (tfCont, acc')
      where
        (acc', sf') = foldr (foldAndFreeze 0) (acc, sf) as
        tfCont = foldrSFAux sf'

    foldrSFAux sf = SF' tf
      where
        tf dt (acc, as) = (tf', acc')
          where
            (acc', sf') = foldr (foldAndFreeze dt) (acc, sf) as
            tf' = foldrSFAux sf'
  
    foldAndFreeze :: DTime -> a -> (b, SF (a, b) b) -> (b, SF (a, b) b)
    foldAndFreeze dt a (b, sf) = (b', sf')
      where
        (sf', b') = runAndFreezeSF sf (a, b) dt

-- TODO: implement using exitsting after
afterExp :: RandomGen g => g -> DTime -> b -> SF a (Event b)
afterExp g t b = SF { sfTF = tf0 }
  where
    (t', _) = randomExp g (1 / t)

    -- there can be no event at time of switching
    tf0 _ = (tfCont, NoEvent)
      where
        tfCont = afterExpAux 0 t'

    afterExpAux tCurr tEvt = SF' tf
      where
        tf dt _ 
            | tCurr' >= tEvt = (tf', Event b)
            | otherwise = (tf', NoEvent)
          where
            tCurr' = tCurr + dt
            tf' = afterExpAux tCurr' tEvt

-- TODO: implement different samling-strategies: random noise, triangle, uniform, predefined sampledistances
superSampling :: Int -> SF a b -> SF a [b]
superSampling n sf0 = SF { sfTF = tf0 }
  where
    -- NOTE: no supersampling at time 0
    tf0 a0 = (tfCont, [b0])
      where
        (sf', b0) = sfTF sf0 a0
        tfCont = superSamplingAux sf'

    superSamplingAux sf' = SF' tf
      where
        tf dt a = (tf', bs)
          where
            (sf'', bs) = superSampleRun n dt sf' a
            tf' = superSamplingAux sf''

    superSampleRun :: Int -> DTime -> SF' a b -> a -> (SF' a b, [b])
    superSampleRun n dt sf a 
        | n <= 1 = superSampleMulti 1 dt sf a []
        | otherwise = (sf', reverse bs)  -- NOTE: need to reverse because need to respect order, use of accumulator reverses them initially
      where
        superDt = dt / fromIntegral n
        (sf', bs) = superSampleMulti n superDt sf a []

    superSampleMulti :: Int -> DTime -> SF' a b -> a -> [b] -> (SF' a b, [b])
    superSampleMulti 0 _ sf _ acc = (sf, acc)
    superSampleMulti n dt sf a acc = superSampleMulti (n-1) dt sf' a (b:acc) 
      where
        (sf', b) = sfTF' sf dt a
-------------------------------------------------------------------------------