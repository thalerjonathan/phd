{-# LANGUAGE Arrows #-}
module FRP.FrABS.Agent.Reactive (
    doOnce,
    transitionAfter
  ) where

import FRP.Yampa

import FRP.FrABS.Agent.Agent

-- study arrowized programming (papers): how can dt disappear ? can we ommit arguments which are implicitly there?
-- develop arrowized EDSL for ABS: timeout transitions, rate transitions, sending messages after, repeatedly send message in interval, occasionally send message

doOnce :: (AgentOut s m ec l -> AgentOut s m ec l) -> SF (AgentOut s m ec l) (AgentOut s m ec l)
doOnce f = proc ao ->
    do
        -- TODO: is this actually evaluated EVERYTIME or due to Haskells laziness just once?
        -- TODO: why is this not firing the first time?
        aoOnceEvt <- once -< (Event . f) ao
        -- this seems to be a bit unelegant, can we formulate this more elegant?
        let ao' = event ao id aoOnceEvt
        returnA -< ao'

transitionAfter :: Double
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
                    -> AgentBehaviour s m ec l
transitionAfter dt from to = switch (transitionAwaiting from) (transitionOccured to)
    where
        transitionAwaiting :: AgentBehaviour s m ec l -> SF (AgentIn s m ec l) (AgentOut s m ec l, Event ())
        transitionAwaiting from = proc ain ->
            do
                ao <- from -< ain
                timeEvent <- after dt () -< ()
                returnA -< (ao, timeEvent)

        transitionOccured :: AgentBehaviour s m ec l -> () -> AgentBehaviour s m ec l
        transitionOccured to _ = to