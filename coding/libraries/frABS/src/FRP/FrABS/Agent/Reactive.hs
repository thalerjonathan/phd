{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive (
    EventSource,
    MessageSource,

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

import Control.Monad.Random

type EventSource s m e = SF (AgentIn s m e, AgentOut s m e) (AgentOut s m e, Event ())
type MessageSource s m e = (AgentOut s m e -> (AgentOut s m e, AgentMessage m))

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
doNothing = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< aout
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
                                        -> SF (AgentOut s m e) (AgentOut s m e)
sendMessageOccasionally g rate msg = sendMessageOccasionallySrc g rate (constMsgSource msg)

sendMessageOccasionallySrc :: RandomGen g => g 
                                        -> Double
                                        -> MessageSource s m e 
                                        -> SF (AgentOut s m e) (AgentOut s m e)
sendMessageOccasionallySrc g rate msgSrc = proc ao ->
    do
        sendEvt <- occasionally g rate () -< ()
        let ao' = sendMessageOccasionallyAux msgSrc sendEvt ao
        returnA -< ao'

    where
        sendMessageOccasionallyAux :: MessageSource s m e 
                                        -> Event () 
                                        -> AgentOut s m e 
                                        -> AgentOut s m e
        sendMessageOccasionallyAux _ NoEvent ao = ao
        sendMessageOccasionallyAux msgSrc (Event ()) ao = sendMessage msg ao'
            where
                (ao', msg) = msgSrc ao

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constMsgReceiverSource :: m -> AgentId -> MessageSource s m e
constMsgReceiverSource m receiver ao = (ao, msg)
    where
        msg = (receiver, m)

constMsgSource :: AgentMessage m -> MessageSource s m e
constMsgSource msg ao = (ao, msg)

randomNeighbourNodeMsgSource :: m -> MessageSource s m (Network l)
randomNeighbourNodeMsgSource m ao = (ao', msg)
    where
        aid = aoId ao
        env = aoEnv ao
        (randNode, ao') = runAgentRandom (pickRandomNeighbourNode aid env) ao
        msg = (randNode, m)

randomNeighbourCellMsgSource :: Discrete2dCoord -> m -> MessageSource s m (Discrete2d AgentId)
randomNeighbourCellMsgSource pos m ao = (ao', msg)
    where
        env = aoEnv ao
        ((_, randCell), ao') = runAgentRandom (pickRandomNeighbourCell pos env) ao
        msg = (randCell, m)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------
transitionAfter :: Double
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionAfter dt from to = switch (transitionAfterAux from) (\_ -> to)
    where
        transitionAfterAux :: AgentBehaviour s m e -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionAfterAux from = proc ain ->
            do
                ao <- from -< ain
                timeoutEvent <- after dt () -< ()
                returnA -< (ao, timeoutEvent)


transitionWithUniProb :: Double
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionWithUniProb p from to = switch (transitionWithUniProbAux from) (\_ -> to)
    where
        transitionWithUniProbAux :: AgentBehaviour s m e
                                    -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionWithUniProbAux from = proc ain ->
            do
                ao <- from -< ain
                let (evtFlag, ao') = runAgentRandom (drawRandomBoolM p) ao
                evt <- iEdge False -< evtFlag
                returnA -< (ao', evt)


transitionWithExpProb :: Double
                        -> Double
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
                        -> AgentBehaviour s m e
transitionWithExpProb lambda p from to = switch (transitionWithExpProbAux from) (\_ -> to)
    where
        transitionWithExpProbAux :: AgentBehaviour s m e
                                    -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionWithExpProbAux from = proc ain ->
            do
                ao <- from -< ain
                let (r, ao') = runAgentRandom (drawRandomExponentialM lambda) ao
                evt <- iEdge False -< (p >= r)
                returnA -< (ao', evt)


transitionOnEvent :: EventSource s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
                    -> AgentBehaviour s m e
transitionOnEvent evtSrc from to = switch (transitionEventAux evtSrc from) (\_ -> to)
    where
        transitionEventAux :: EventSource s m e
                                -> AgentBehaviour s m e
                                -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionEventAux evtSrc from = proc ain ->
            do
                ao <- from -< ain
                (ao', evt) <- evtSrc -< (ain, ao)
                returnA -< (ao', evt)


transitionOnBoolState :: (s -> Bool)
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
                            -> AgentBehaviour s m e
transitionOnBoolState boolStateFunc from to = switch (transitionOnBoolStateAux boolStateFunc from) (\_ -> to)
    where
        transitionOnBoolStateAux :: (s -> Bool)
                                    -> AgentBehaviour s m e
                                    -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionOnBoolStateAux boolStateFunc from = proc ain ->
            do
                ao <- from -< ain
                let state = aoState ao
                let evtFlag = boolStateFunc state
                evt <- iEdge False -< evtFlag
                returnA -< (ao, evt)


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
                                        -> SF (AgentIn s m e) (AgentOut s m e, Event ())
        transitionEventWithGuardAux evtSrc from = proc ain ->
            do
                ao <- from -< ain
                (ao0, evt) <- evtSrc -< (ain, ao)
                let (ao1, transEvt) = guardEvent evt guardAction ao0
                returnA -< (ao1, transEvt)

        guardEvent :: Event () -> Rand StdGen Bool -> AgentOut s m e -> (AgentOut s m e, Event ())
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
messageEventSource :: (Eq m) => m -> EventSource s m e
messageEventSource msg = proc (ain, ao) ->
    do
        evt <- iEdge False -< hasMessage msg ain
        returnA -< (ao, evt)
-------------------------------------------------------------------------------