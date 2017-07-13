{-# LANGUAGE Arrows #-}
module Wildfire.Agent (
	wildfireAgentBehaviour
  ) where

import Wildfire.Model

import FRP.FrABS

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive Functions
------------------------------------------------------------------------------------------------------------------------
isBurnedDown :: WildfireAgentState -> Bool
isBurnedDown s = wfFuelCurr s <= 0.0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
-- DEAD
wildfireAgentDie :: WildfireAgentBehaviour
wildfireAgentDie = proc ain ->
	do
		let aout = agentOutFromIn ain
		let aout' = updateDomainState (\s -> s { wfLifeState = Dead }) aout
		returnA -< kill aout' -- NOTE: killing leads to increased performance but leaves the patch in the color of the background

-- BURNING
wildfireAgentBurning :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBurning g initFuel = transitionOnBoolState
									isBurnedDown
									(wildfireAgentBurningBehaviour g initFuel)
									wildfireAgentDie

wildfireAgentBurningBehaviour :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g initFuel = proc ain -> 
	do
		let ao = agentOutFromIn ain
		ao0 <- doOnce (updateDomainState (\s -> s { wfLifeState = Burning })) -< ao
		ao1 <- burndown initFuel -< ao0
		ao2 <- igniteNeighbours g -< ao1
		returnA -< ao2

burndown :: Double -> SF WildfireAgentOut WildfireAgentOut
burndown initFuel = proc ao ->	
	do
		currFuel <- drain initFuel -< wfFuelRate $ aoState ao
		let ao' = updateDomainState (\s -> s { wfFuelCurr = currFuel }) ao
		returnA -< ao'

igniteNeighbours :: RandomGen g => g -> SF WildfireAgentOut WildfireAgentOut
igniteNeighbours g = sendMessageOccasionallySrc 
							g 
							(1 / ignitions)
							(randomNeighbourCellMsgSource Ignite)

-- LIVING
wildfireAgentLiving :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentLiving g initFuel = transitionOnMessage 
									Ignite
									doNothing
									(wildfireAgentBurning g initFuel)

-- NOTE: this implementation has a probabilistic Ignition which depends on how 'burnable' (=initialFuel) the agent (cell) is
wildfireAgentLivingGuarded :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentLivingGuarded g initFuel = transitionOnEventWithGuard 
											(messageEventSource Ignite)
											(drawRandomBoolM initFuel)
											doNothing
											(wildfireAgentBurning g initFuel)

-- INITIAL
wildfireAgentBehaviour :: RandomGen g => g -> Double -> WildfireAgentBehaviour
wildfireAgentBehaviour = wildfireAgentLivingGuarded
------------------------------------------------------------------------------------------------------------------------