{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive (
    doOnce,
    transitionAfter,
    transitionEventWithGuard
  ) where

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Random

import Control.Monad.Random
import Control.Monad.Trans.State


doOnce :: (AgentOut s m ec l -> AgentOut s m ec l) -> SF (AgentOut s m ec l) (AgentOut s m ec l)
doOnce f = proc ao ->
    do
        -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
        -- TODO: why is this not firing the first time?
        aoOnceEvt <- once -< (Event . f) ao
        -- this seems to be a bit unelegant, can we formulate this more elegant?
        let ao' = event ao id aoOnceEvt
        returnA -< ao'

transitionEventWithGuard :: SF (AgentIn s m ec l) (Event ())
                            -> Rand StdGen Bool
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
                            -> AgentBehaviour s m ec l
transitionEventWithGuard f guardAction from to = switch (transitionEventWithGuardAux f from) (\_ -> to)
    where
        transitionEventWithGuardAux :: SF (AgentIn s m ec l) (Event ())
                                        -> AgentBehaviour s m ec l 
                                        -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionEventWithGuardAux f from = proc ain ->
            do
                ao <- from -< ain
                evt <- f -< ain

                let (ao', transEvt) = guardEvent evt guardAction ao

                returnA -< (ao', transEvt)

        guardEvent :: Event () -> Rand StdGen Bool -> AgentOut s m ec l -> (AgentOut s m ec l, Event ())
        guardEvent NoEvent _ ao = (ao, NoEvent)
        guardEvent _ guardAction ao 
            | guardAllowed = (ao', Event ())
            | otherwise = (ao', NoEvent)
            where
                (guardAllowed, ao') = runAgentRandom guardAction ao

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