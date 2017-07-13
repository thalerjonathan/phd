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
isBurnedDown s = wfFuel s <= 0.0
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
wildfireAgentBurning :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBurning g = transitionOnBoolState
							isBurnedDown
							(wildfireAgentBurningBehaviour g)
							wildfireAgentDie

wildfireAgentBurningBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBurningBehaviour g = proc ain -> 
	do
		let ao = agentOutFromIn ain
		ao0 <- burndown -< ao
		ao1 <- igniteNeighbours g -< ao0
		returnA -< ao1

igniteNeighbours :: RandomGen g => g -> SF WildfireAgentOut WildfireAgentOut
igniteNeighbours g = sendMessageOccasionallySrc 
							g 
							0.2
							(randomNeighbourCellMsgSource Ignite)

burndown :: SF WildfireAgentOut WildfireAgentOut
burndown = proc a ->
	do
		let s = aoState a
		let fuel = wfFuel s
		remainingFuel <- (1.0-) ^<< integral -< 1.0 -- TODO: how can we put fuel-variable in?
		returnA -< updateDomainState (\s -> s { wfLifeState = Burning, wfFuel = max 0.0 remainingFuel}) a

-- LIVING
wildfireAgentLiving :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentLiving g = transitionOnMessage 
							Ignite
							doNothing
							(wildfireAgentBurning g)

-- INITIAL
wildfireAgentBehaviour :: RandomGen g => g -> WildfireAgentBehaviour
wildfireAgentBehaviour = wildfireAgentLiving
------------------------------------------------------------------------------------------------------------------------