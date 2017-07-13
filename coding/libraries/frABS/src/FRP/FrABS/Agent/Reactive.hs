{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive (
    EventSource,
    MessageSource,

    doOnce,
    
    sendMessageOccasionallySrc,
    sendMessageOccasionally,

    constMsgReceiverSource,
    constMsgSource,
    randomNodeMsgSource,

    transitionAfter,
    transitionWithUniProb,
    transitionWithExpProb,
    transitionOnEvent,
    transitionOnMessage,
    transitionOnEventWithGuard,

    messageEventSource
  ) where

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Random
import FRP.FrABS.Agent.Utils

import Control.Monad.Random

type EventSource s m ec l = SF (AgentIn s m ec l, AgentOut s m ec l) (AgentOut s m ec l, Event ())
type MessageSource s m ec l = (AgentOut s m ec l -> (AgentOut s m ec l, AgentMessage m))

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------
doOnce :: (AgentOut s m ec l -> AgentOut s m ec l) -> SF (AgentOut s m ec l) (AgentOut s m ec l)
doOnce f = proc ao ->
    do
        -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
        -- TODO: why is this not firing the first time?
        aoOnceEvt <- once -< (Event . f) ao
        -- this seems to be a bit unelegant, can we formulate this more elegant?
        let ao' = event ao id aoOnceEvt
        returnA -< ao'
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
                                        -> SF (AgentOut s m ec l) (AgentOut s m ec l)
sendMessageOccasionally g rate msg = sendMessageOccasionallySrc g rate (constMsgSource msg)

sendMessageOccasionallySrc :: RandomGen g => g 
                                        -> Double
                                        -> MessageSource s m ec l 
                                        -> SF (AgentOut s m ec l) (AgentOut s m ec l)
sendMessageOccasionallySrc g rate msgSrc = proc ao ->
    do
        sendEvt <- occasionally g rate () -< ()
        let ao' = sendMessageOccasionallyAux msgSrc sendEvt ao
        returnA -< ao'

    where
        sendMessageOccasionallyAux :: MessageSource s m ec l 
                                        -> Event () 
                                        -> AgentOut s m ec l 
                                        -> AgentOut s m ec l
        sendMessageOccasionallyAux _ NoEvent ao = ao
        sendMessageOccasionallyAux msgSrc (Event ()) ao = sendMessage msg ao'
            where
                (ao', msg) = msgSrc ao

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constMsgReceiverSource :: m -> AgentId -> MessageSource s m ec l
constMsgReceiverSource m receiver ao = (ao, msg)
    where
        msg = (receiver, m)

constMsgSource :: AgentMessage m -> MessageSource s m ec l
constMsgSource msg ao = (ao, msg)

randomNodeMsgSource :: m -> MessageSource s m ec l
randomNodeMsgSource m ao = (ao', msg)
    where
        (randNode, ao') = runAgentRandom (pickRandomNeighbourNode ao) ao
        msg = (randNode, m)

-- TODO: can we do the following? problem: ec must be AgentId, but this is not allowed by the compiler, cannot infer the type
{- 
randomCellMsgSource :: m -> MessageSource s m ec l
randomCellMsgSource m ao = (ao', msg)
    where
        ((_, randCell), ao') = runAgentRandom (pickRandomNeighbourCell ao) ao
        msg = (randCell, m)
-}
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------
transitionAfter :: Double
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
transitionAfter dt from to = switch (transitionAfterAux from) (\_ -> to)
    where
        transitionAfterAux :: AgentBehaviour s m ec l -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionAfterAux from = proc ain ->
            do
                ao <- from -< ain
                timeoutEvent <- after dt () -< ()
                returnA -< (ao, timeoutEvent)

transitionWithUniProb :: Double
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
transitionWithUniProb p from to = switch (transitionWithUniProbAux from) (\_ -> to)
    where
        transitionWithUniProbAux :: AgentBehaviour s m ec l
                                    -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionWithUniProbAux from = proc ain ->
            do
                ao <- from -< ain
                let (evtFlag, ao') = runAgentRandom (drawRandomBoolM p) ao
                evt <- edge -< evtFlag
                returnA -< (ao', evt)

transitionWithExpProb :: Double
                        -> Double
                        -> AgentBehaviour s m ec l
                        -> AgentBehaviour s m ec l
                        -> AgentBehaviour s m ec l
transitionWithExpProb lambda p from to = switch (transitionWithExpProbAux from) (\_ -> to)
    where
        transitionWithExpProbAux :: AgentBehaviour s m ec l
                                    -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionWithExpProbAux from = proc ain ->
            do
                ao <- from -< ain
                let (r, ao') = runAgentRandom (drawRandomExponentialM lambda) ao
                evt <- edge -< (p >= r)
                returnA -< (ao', evt)

transitionOnEvent :: EventSource s m ec l
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (\_ -> to)
    where
        transitionEventAux :: EventSource s m ec l
                                -> AgentBehaviour s m ec l
                                -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionEventAux evtSrc from = proc ain ->
            do
                ao <- from -< ain
                (ao', evt) <- evtSrc -< (ain, ao)
                returnA -< (ao', evt)

transitionOnMessage :: (Eq m) => m 
                        -> AgentBehaviour s m ec l
                        -> AgentBehaviour s m ec l
                        -> AgentBehaviour s m ec l
transitionOnMessage msg from to = transitionOnEvent (messageEventSource msg) from to


transitionOnEventWithGuard :: EventSource s m ec l
                            -> Rand StdGen Bool
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
transitionOnEventWithGuard evtSrc guardAction from to = switch (transitionEventWithGuardAux evtSrc from) (\_ -> to)
    where
        transitionEventWithGuardAux :: EventSource s m ec l
                                        -> AgentBehaviour s m ec l 
                                        -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionEventWithGuardAux evtSrc from = proc ain ->
            do
                ao <- from -< ain
                (ao0, evt) <- evtSrc -< (ain, ao)
                let (ao1, transEvt) = guardEvent evt guardAction ao0
                returnA -< (ao1, transEvt)

        guardEvent :: Event () -> Rand StdGen Bool -> AgentOut s m ec l -> (AgentOut s m ec l, Event ())
        guardEvent NoEvent _ ao = (ao, NoEvent)
        guardEvent _ guardAction ao 
            | guardAllowed = (ao', Event ())
            | otherwise = (ao', NoEvent)
            where
                (guardAllowed, ao') = runAgentRandom guardAction ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- EVENT-Sources
-------------------------------------------------------------------------------
-- TODO: how can we formulate this in point-free arrow style?
messageEventSource :: (Eq m) => m -> EventSource s m ec l
messageEventSource msg = proc (ain, ao) ->
    do
        evt <- edge -< hasMessage msg ain
        returnA -< (ao, evt)
-------------------------------------------------------------------------------