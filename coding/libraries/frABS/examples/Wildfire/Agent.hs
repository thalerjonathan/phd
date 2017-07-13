{-# LANGUAGE Arrows #-}
module Wildfire.Agent (
	wildfireAgentLivingBehaviour
  ) where

import Wildfire.Model

import FRP.FrABS

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
igniteNeighbours :: WildfireAgentOut -> WildfireAgentOut
igniteNeighbours ao = sendMessage (n, Ignite) a'
	where
		nids = neighbourCells ao
		(n, a') = agentPickRandom nids ao


isBurnedDown :: WildfireAgentOut -> Bool
isBurnedDown ao = wfFuel s <= 0.0
	where
		s = aoState ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
igniteNeighboursSF :: RandomGen g => g -> SF WildfireAgentOut WildfireAgentOut
igniteNeighboursSF g = proc a-> 
	do
		ignitionEvent <- occasionally g 0.2 () -< ()
		returnA -< (event a (\_ -> igniteNeighbours a) ignitionEvent)

burndownSF :: SF WildfireAgentOut WildfireAgentOut
burndownSF = proc a ->
	do
		let s = aoState a
		let fuel = wfFuel s
		remainingFuel <- (1.0-) ^<< integral -< 1.0 -- TODO: how can we put fuel-variable in?
		returnA -< updateDomainState (\s -> s { wfLifeState = Burning, wfFuel = max 0.0 remainingFuel}) a

wildfireAgentLiving :: SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentLiving = proc ain ->
	do
		let aout = agentOutFromIn ain
		ignitionEvent <- (iEdge False) -< hasMessage Ignite ain 	-- NOTE: need to use iEdge False to detect Ignite messages which were added at time 0: at initialization time
		returnA -< (aout, ignitionEvent)

wildfireAgentIgnite :: RandomGen g => g -> () -> WildfireAgentBehaviour
wildfireAgentIgnite g _ = wildfireAgentBurningBehaviour g

wildfireAgentDie :: () -> WildfireAgentBehaviour
wildfireAgentDie _ = proc ain ->
	do
		let aout = agentOutFromIn ain
		let aout' = updateDomainState (\s -> s { wfLifeState = Dead }) aout
		returnA -< aout' -- kill aout' -- NOTE: killing would lead to increased performance but would leave the patch white (blank)

wildfireAgentBurning :: RandomGen g => g -> SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentBurning g = proc ain -> 
	do
		let a = agentOutFromIn ain

		a' <- burndownSF -< a
		a'' <- igniteNeighboursSF g -< a'

		dyingEvent <- edge -< isBurnedDown a''

		returnA -< (a'', dyingEvent)

-- TODO: use transitionAfter
wildfireAgentBurningBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g = switch (wildfireAgentBurning g) wildfireAgentDie

wildfireAgentLivingBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentLivingBehaviour g = switch wildfireAgentLiving (wildfireAgentIgnite g)
------------------------------------------------------------------------------------------------------------------------