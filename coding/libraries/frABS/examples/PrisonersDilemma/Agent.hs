{-# LANGUAGE Arrows #-}
module PrisonersDilemma.Agent (
	pdAgentBehaviour
  ) where

import PrisonersDilemma.Model

import FRP.FrABS

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
-- Non-Reactive functions
------------------------------------------------------------------------------------------------------------------------
payoff :: PDAction -> PDAction -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam

broadcastLocalAction :: PDAgentOut -> PDAgentOut
broadcastLocalAction ao = broadcastMessage (NeighbourAction curr) ns ao
	where
		curr = pdCurrAction $ aoState ao
		ns = neighbourCells ao

broadcastLocalPayoff :: PDAgentOut -> PDAgentOut
broadcastLocalPayoff ao = broadcastMessage (NeighbourPayoff (currAct, currPo)) ns ao
	where
		s = aoState ao
		currAct = pdCurrAction s
		currPo = pdLocalPo s
		ns = neighbourCells ao

handleNeighbourAction :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourAction ain ao = onMessage handleNeighbourActionAux ain ao
	where
		handleNeighbourActionAux :: PDAgentOut -> AgentMessage PDMsg -> PDAgentOut
		handleNeighbourActionAux ao (_, NeighbourAction act) = updateDomainState (\s -> s { pdLocalPo = pdLocalPo s + po }) ao
			where
				curr = pdCurrAction $ aoState ao
				po = payoff curr act
		handleNeighbourActionAux ao _ = ao

handleNeighbourPayoff :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourPayoff ain ao = onMessage handleNeighbourPayoffAux ain ao
	where
		handleNeighbourPayoffAux :: PDAgentOut -> AgentMessage PDMsg -> PDAgentOut
		handleNeighbourPayoffAux ao (_, NeighbourPayoff po@(poAct, poValue))
			| poValue > localBestPoValue = updateDomainState (\s -> s { pdBestPo = po }) ao
			| otherwise = ao
			where
				s = aoState ao
				(_, localBestPoValue) = pdBestPo s

		handleNeighbourPayoffAux ao _ = ao

switchToBestPayoff :: PDAgentOut -> PDAgentOut
switchToBestPayoff ao = 
	updateDomainState (\s -> s { 
		pdCurrAction = bestAction,
    	pdPrevAction = oldAction,
    	pdLocalPo = 0.0,
    	pdBestPo = (bestAction, 0.0)})
		ao

	where
		s = aoState ao
		oldAction = pdCurrAction s
		(bestAction, _) = pdBestPo s

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
pdAgentAwaitingNeighbourPayoffs :: PDAgentBehaviour
pdAgentAwaitingNeighbourPayoffs = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao0 = handleNeighbourPayoff ain ao

		ao1 <- doOnce broadcastLocalPayoff -< ao0

		returnA -< ao1

pdAgentAwaitingNeighbourActions :: PDAgentBehaviour
pdAgentAwaitingNeighbourActions = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao0 = handleNeighbourAction ain ao

		ao1 <- doOnce (broadcastLocalAction . switchToBestPayoff) -< ao0

		returnA -< ao1

pdAgentWaitForActions :: PDAgentBehaviour
pdAgentWaitForActions = transitionAfter 
							halfRoundTripTime 
							pdAgentAwaitingNeighbourActions 
							pdAgentWaitForPayoffs

pdAgentWaitForPayoffs :: PDAgentBehaviour
pdAgentWaitForPayoffs = transitionAfter 
							halfRoundTripTime 
							pdAgentAwaitingNeighbourPayoffs 
							pdAgentWaitForActions

pdAgentBehaviour :: PDAgentBehaviour
pdAgentBehaviour = pdAgentWaitForActions
------------------------------------------------------------------------------------------------------------------------ 