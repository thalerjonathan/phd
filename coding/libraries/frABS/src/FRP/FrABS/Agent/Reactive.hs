{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive (
    EventSource,
    MessageSource,

    ReactiveBehaviourIgnoreEnv,

    ignoreEnv,

    drain,
    
    doOnce,
    doNothing,
    
    sendMessageOccasionallySrc,
    sendMessageOccasionally,

    constMsgReceiverSource,
    constMsgSource,
    randomNeighbourNodeMsgSource,
    randomNeighbourCellMsgSource,
    
    transitionAfter,
    transitionAfterExp,
    transitionWithUniProb,
    transitionWithExpProb,
    transitionOnEvent,
    transitionOnMessage,
    transitionOnEventWithGuard,
    transitionOnBoolState,

    messageEventSource
  ) where

import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Network

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Random

import FRP.Yampa
import FRP.Yampa.InternalCore

import Control.Monad.Random

-- TODO: is access to environment necesssary here?
type EventSource s m e = SF (AgentIn s m e, AgentOut s m e) (AgentOut s m e, Event ())
type MessageSource s m e = (e -> AgentOut s m e -> (AgentOut s m e, AgentMessage m))

type ReactiveBehaviourIgnoreEnv s m e = SF (AgentIn s m e) (AgentOut s m e)

-------------------------------------------------------------------------------
-- MISC
-------------------------------------------------------------------------------
ignoreEnv :: ReactiveBehaviourIgnoreEnv s m e -> AgentBehaviour s m e 
ignoreEnv f = proc (ain, e) ->
    do
        ao <- f -< ain
        returnA -< (ao, e)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Continuous Helpers
-------------------------------------------------------------------------------
drain :: Double -> SF Double Double
drain initValue = proc rate ->
    do
        value <- (initValue-) ^<< integral -< rate
        let value' = max value 0
        returnA -< value'
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------
doOnce :: (AgentOut s m e -> AgentOut s m e) -> SF (AgentOut s m e) (AgentOut s m e)
doOnce f = proc ao ->
    do
        -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
        -- TODO: why is this not firing the first time?
        aoOnceEvt <- once -< (Event . f) ao
        -- this seems to be a bit unelegant, can we formulate this more elegant?
        let ao' = event ao id aoOnceEvt
        returnA -< ao'

-- TODO: can we formulate this in one line, point-free?
doNothing :: AgentBehaviour s m e
doNothing = proc (ain, e) ->
    do
        let aout = agentOutFromIn ain
        returnA -< (aout, e)
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
                                        -> SF (AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionally g rate msg = sendMessageOccasionallySrc g rate (constMsgSource msg)

sendMessageOccasionallySrc :: RandomGen g => g 
                                        -> Double
                                        -> MessageSource s m e 
                                        -> SF (AgentOut s m e, e) (AgentOut s m e)
sendMessageOccasionallySrc g rate msgSrc = proc aoe ->
    do
        sendEvt <- occasionally g rate () -< ()
        let ao' = sendMessageOccasionallyAux msgSrc sendEvt aoe
        returnA -< ao'

    where
        sendMessageOccasionallyAux :: MessageSource s m e 
                                        -> Event () 
                                        -> (AgentOut s m e, e)
                                        -> AgentOut s m e
        sendMessageOccasionallyAux _ NoEvent (ao, _) = ao
        sendMessageOccasionallyAux msgSrc (Event ()) (ao, e) = sendMessage msg ao'
            where
                (ao', msg) = msgSrc e ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constMsgReceiverSource :: m -> AgentId -> MessageSource s m e
constMsgReceiverSource m receiver _ ao = (ao, msg)
    where
        msg = (receiver, m)

constMsgSource :: AgentMessage m -> MessageSource s m e
constMsgSource msg _ ao = (ao, msg)

randomNeighbourNodeMsgSource :: m -> MessageSource s m (Network l)
randomNeighbourNodeMsgSource m e ao = (ao', msg)
    where
        aid = aoId ao
        (randNode, ao') = agentRandom (randomNeighbourNode aid e) ao
        msg = (randNode, m)

randomNeighbourCellMsgSource :: (s -> Discrete2dCoord) -> m -> Bool -> MessageSource s m (Discrete2d AgentId)
randomNeighbourCellMsgSource posFunc m ic e ao = (ao', msg)
    where
        pos = posFunc $ aoState ao
        (randCell, ao') = agentRandom (randomNeighbourCell pos ic e) ao
        msg = (randCell, m)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------
transitionAfter :: Double
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfter t from to = switch (transitionAfterAux t from) (\_ -> to)
    where
        transitionAfterAux :: Double 
                                -> AgentBehaviour s m e 
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionAfterAux t from = proc aie ->
            do
                aoe <- from -< aie
                timeoutEvent <- after t () -< ()
                returnA -< (aoe, timeoutEvent)


transitionAfterExp :: RandomGen g =>
                    g 
                    -> Double
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfterExp g t from to = switch (transitionAfterExpAux t from) (\_ -> to)
    where
        transitionAfterExpAux :: Double 
                                -> AgentBehaviour s m e 
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionAfterExpAux t from = proc aie ->
            do
                aoe <- from -< aie
                --timeoutEvent <- afterExp g t () -< ()
                timeoutEvent <- occasionally g t () -< ()
                returnA -< (aoe, timeoutEvent)

afterExp :: RandomGen g => g -> DTime -> b -> SF a (Event b)
afterExp g t b = SF { sfTF = tf0 }
    where
        (t', g') = randomExp g (1 / t)

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
                        tCurr' = (tCurr + dt)
                        tf' = afterExpAux tCurr' tEvt


transitionWithUniProb :: Double
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionWithUniProb p from to = switch (transitionWithUniProbAux from) (\_ -> to)
    where
        transitionWithUniProbAux :: AgentBehaviour s m e
                                    -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionWithUniProbAux from = proc aie ->
            do
                (ao, e') <- from -< aie
                let (evtFlag, ao') = agentRandom (randomBoolM p) ao
                evt <- iEdge False -< evtFlag
                returnA -< ((ao', e'), evt)


transitionWithExpProb :: Double
                        -> Double
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
transitionWithExpProb lambda p from to = switch (transitionWithExpProbAux from) (\_ -> to)
    where
        transitionWithExpProbAux :: AgentBehaviour s m e
                                    -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionWithExpProbAux from = proc aie ->
            do
                (ao, e) <- from -< aie
                let (r, ao') = agentRandom (randomExpM lambda) ao
                evt <- iEdge False -< (p >= r)
                returnA -< ((ao', e), evt)


transitionOnEvent :: EventSource s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (\_ -> to)
    where
        transitionEventAux :: EventSource s m e
                                -> AgentBehaviour s m e
                                -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionEventAux evtSrc from = proc aie@(ain, _) ->
            do
                (ao, e) <- from -< aie
                (ao', evt) <- evtSrc -< (ain, ao)
                returnA -< ((ao', e), evt)


transitionOnBoolState :: (s -> Bool)
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionOnBoolState boolStateFunc from to = switch (transitionOnBoolStateAux boolStateFunc from) (\_ -> to)
    where
        transitionOnBoolStateAux :: (s -> Bool)
                                    -> AgentBehaviour s m e
                                    -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionOnBoolStateAux boolStateFunc from = proc aie ->
            do
                (ao, e) <- from -< aie
                let state = aoState ao
                let evtFlag = boolStateFunc state
                evt <- iEdge False -< evtFlag
                returnA -< ((ao, e), evt)


transitionOnMessage :: (Eq m) => m 
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
transitionOnMessage msg from to = transitionOnEvent (messageEventSource msg) from to


transitionOnEventWithGuard :: EventSource s m e
                            -> Rand StdGen Bool
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionOnEventWithGuard evtSrc guardAction from to = switch (transitionEventWithGuardAux evtSrc from) (\_ -> to)
    where
        transitionEventWithGuardAux :: EventSource s m e
                                        -> AgentBehaviour s m e 
                                        -> SF (AgentIn s m e, e) ((AgentOut s m e, e), Event ())
        transitionEventWithGuardAux evtSrc from = proc aie@(ain, e) ->
            do
                (ao, e') <- from -< aie
                (ao0, evt) <- evtSrc -< (ain, ao)
                let (ao1, transEvt) = guardEvent evt guardAction ao0
                returnA -< ((ao1, e), transEvt)

        guardEvent :: Event () -> Rand StdGen Bool -> AgentOut s m e -> (AgentOut s m e, Event ())
        guardEvent NoEvent _ ao = (ao, NoEvent)
        guardEvent _ guardAction ao 
            | guardAllowed = (ao', Event ())
            | otherwise = (ao', NoEvent)
            where
                (guardAllowed, ao') = agentRandom guardAction ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- EVENT-Sources
-------------------------------------------------------------------------------
-- TODO: how can we formulate this in point-free arrow style?
messageEventSource :: (Eq m) => m -> EventSource s m e
messageEventSource msg = proc (ain, ao) ->
    do
        evt <- iEdge False -< hasMessage msg ain
        returnA -< (ao, evt)
-------------------------------------------------------------------------------