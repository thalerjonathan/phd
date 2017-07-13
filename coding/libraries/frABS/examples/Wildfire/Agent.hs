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
isBurnedDown :: WildfireAgentOut -> Bool
isBurnedDown ao = wfFuel s <= 0.0
	where
		s = aoState ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
igniteNeighbours :: RandomGen g => g -> SF WildfireAgentOut WildfireAgentOut
igniteNeighbours g = sendMessageOccasionallySrc 
							g 
							0.2
							(randomNeighbourCellMsgSource Ignite)

burndownSF :: SF WildfireAgentOut WildfireAgentOut
burndownSF = proc a ->
	do
		let s = aoState a
		let fuel = wfFuel s
		remainingFuel <- (1.0-) ^<< integral -< 1.0 -- TODO: how can we put fuel-variable in?
		returnA -< updateDomainState (\s -> s { wfLifeState = Burning, wfFuel = max 0.0 remainingFuel}) a

wildfireAgentLiving :: WildfireAgentBehaviour
wildfireAgentLiving = proc ain ->
	do
		let aout = agentOutFromIn ain
		returnA -< aout

wildfireAgentDie :: () -> WildfireAgentBehaviour
wildfireAgentDie _ = proc ain ->
	do
		let aout = agentOutFromIn ain
		let aout' = updateDomainState (\s -> s { wfLifeState = Dead }) aout
		returnA -< kill aout' -- NOTE: killing would lead to increased performance but would leave the patch white (blank)

wildfireAgentBurning :: RandomGen g => g -> SF WildfireAgentIn (WildfireAgentOut, Event ())
wildfireAgentBurning g = proc ain -> 
	do
		let a = agentOutFromIn ain

		a0 <- burndownSF -< a
		a1 <- igniteNeighbours g -< a0
		
		dyingEvent <- edge -< isBurnedDown a1

		returnA -< (a1, dyingEvent)

wildfireAgentBurningBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g = switch (wildfireAgentBurning g) wildfireAgentDie

wildfireAgentLivingBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentLivingBehaviour g = transitionOnMessage 
									Ignite
									wildfireAgentLiving 
									(wildfireAgentBurningBehaviour g)
------------------------------------------------------------------------------------------------------------------------