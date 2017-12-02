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

broadcastLocalAction :: PDEnvironment -> PDAgentOut -> PDAgentOut
broadcastLocalAction e ao = broadcastMessage (NeighbourAction curr) ns ao
	where
		s = agentState ao

		coord = pdCoord s
		curr = pdCurrAction s

		ns = neighbourCells coord True e -- NOTE: include self, otherwise will lead to wrong results

broadcastLocalPayoff :: PDEnvironment -> PDAgentOut -> PDAgentOut
broadcastLocalPayoff e ao = broadcastMessage (NeighbourPayoff (currAct, currPo)) ns ao
	where
		s = agentState ao

		coord = pdCoord s
		currAct = pdCurrAction s
		currPo = pdLocalPo s

		ns = neighbourCells coord True e -- NOTE: include self, otherwise will lead to wrong results

handleNeighbourAction :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourAction ain ao = onMessage handleNeighbourActionAux ain ao
	where
		handleNeighbourActionAux :: AgentMessage PDMsg -> PDAgentOut -> PDAgentOut
		handleNeighbourActionAux (_, NeighbourAction act) ao = updateAgentState (\s -> s { pdLocalPo = pdLocalPo s + po }) ao
			where
				curr = pdCurrAction $ agentState ao
				po = payoff curr act
		handleNeighbourActionAux _ ao = ao

handleNeighbourPayoff :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourPayoff ain ao = onMessage handleNeighbourPayoffAux ain ao
	where
		handleNeighbourPayoffAux :: AgentMessage PDMsg -> PDAgentOut -> PDAgentOut
		handleNeighbourPayoffAux (_, NeighbourPayoff po@(poAct, poValue)) ao
			| poValue > localBestPoValue = updateAgentState (\s -> s { pdBestPo = po }) ao
			| otherwise = ao
			where
				s = agentState ao
				(_, localBestPoValue) = pdBestPo s

		handleNeighbourPayoffAux _ ao = ao

switchToBestPayoff :: PDAgentOut -> PDAgentOut
switchToBestPayoff ao = 
	updateAgentState (\s -> s { 
		pdCurrAction = bestAction,
    	pdPrevAction = oldAction,
    	pdLocalPo = 0.0,
    	pdBestPo = (bestAction, 0.0)})
		ao

	where
		s = agentState ao
		oldAction = pdCurrAction s
		(bestAction, _) = pdBestPo s

------------------------------------------------------------------------------------------------------------------------
-- Reactive Functions
------------------------------------------------------------------------------------------------------------------------
pdAgentAwaitingNeighbourPayoffs :: PDEnvironment -> PDAgentBehaviourIgnoreEnv
pdAgentAwaitingNeighbourPayoffs e = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao0 = handleNeighbourPayoff ain ao

		ao1 <- doOnce (broadcastLocalPayoff e) -< ao0

		returnA -< ao1

pdAgentAwaitingNeighbourActions :: PDEnvironment ->  PDAgentBehaviourIgnoreEnv
pdAgentAwaitingNeighbourActions e = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao0 = handleNeighbourAction ain ao

		ao1 <- doOnce ((broadcastLocalAction e) . switchToBestPayoff) -< ao0

		returnA -< ao1

pdAgentWaitForActions :: PDEnvironment -> PDAgentBehaviour
pdAgentWaitForActions e = transitionAfter 
							halfRoundTripTime 
							(ignoreEnv (pdAgentAwaitingNeighbourActions e))
							(pdAgentWaitForPayoffs e)

pdAgentWaitForPayoffs :: PDEnvironment -> PDAgentBehaviour
pdAgentWaitForPayoffs e = transitionAfter 
							halfRoundTripTime 
							(ignoreEnv (pdAgentAwaitingNeighbourPayoffs e))
							(pdAgentWaitForActions e)

pdAgentBehaviour :: PDEnvironment -> PDAgentBehaviour
pdAgentBehaviour = pdAgentWaitForActions
------------------------------------------------------------------------------------------------------------------------ 