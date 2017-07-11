{-# LANGUAGE Arrows #-}
module PrisonersDilemma.Agent (
	pdAgentBehaviour
  ) where

import PrisonersDilemma.Model

import FRP.FrABS

import FRP.Yampa

------------------------------------------------------------------------------------------------------------------------
payoff :: PDAction -> PDAction -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam

neighbourIds :: PDAgentOut -> [AgentId]
neighbourIds ao = map snd neighs
	where
		env = aoEnv ao
		coord = aoEnvPos ao
		neighs = neighbours env coord

broadcastLocalAction :: PDAgentOut -> PDAgentOut
broadcastLocalAction a = broadcastMessage a (NeighbourAction curr) ns
	where
		curr = pdCurrAction $ aoState a
		ns = neighbourIds a

broadcastLocalPayoff :: PDAgentOut -> PDAgentOut
broadcastLocalPayoff a = broadcastMessage a (NeighbourPayoff (currAct, currPo)) ns
	where
		s = aoState a
		currAct = pdCurrAction s
		currPo = pdLocalPo s
		ns = neighbourIds a

handleNeighbourAction :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourAction ain ao = onMessage ain handleNeighbourActionAux ao
	where
		handleNeighbourActionAux :: PDAgentOut -> AgentMessage PDMsg -> PDAgentOut
		handleNeighbourActionAux ao (_, (NeighbourAction act)) = updateDomainState ao (\s -> s { pdLocalPo = pdLocalPo s + po })
			where
				curr = pdCurrAction $ aoState ao
				po = payoff curr act
		handleNeighbourActionAux ao _ = ao

handleNeighbourPayoff :: PDAgentIn -> PDAgentOut -> PDAgentOut
handleNeighbourPayoff ain ao = onMessage ain handleNeighbourPayoffAux ao
	where
		handleNeighbourPayoffAux :: PDAgentOut -> AgentMessage PDMsg -> PDAgentOut
		handleNeighbourPayoffAux ao (_, (NeighbourPayoff po@(poAct, poValue)))
			| poValue > localPoValue = updateDomainState ao (\s -> s { pdBestPo = po })
			| otherwise = ao
			where
				s = aoState ao
				localPoValue = pdLocalPo s

		handleNeighbourPayoffAux ao _ = ao

switchToBestPayoff :: PDAgentOut -> PDAgentOut
switchToBestPayoff ao = 
	updateDomainState ao (\s -> s { 
		pdCurrAction = bestAction,
    	pdPrevAction = oldAction,
    
    	pdLocalPo = 0.0,

    	pdBestPo = (bestAction, 0.0)})
	where
		s = aoState ao
		oldAction = pdCurrAction s
		(bestAction, _) = pdBestPo s


pdAgentAwaitingNeighbourPayoffs :: SF PDAgentIn (PDAgentOut, Event ())
pdAgentAwaitingNeighbourPayoffs = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao' = handleNeighbourPayoff ain ao

		timeEvent <- after 1.0 () -< ()	-- NOTE: can also replace this by counting the number of payoffs received from neighbours and creating an event when all have been received
		returnA -< (ao', timeEvent)

pdAgentNeighbourPayoffsReceived :: () -> PDAgentBehaviour
pdAgentNeighbourPayoffsReceived _ = -- TODO: switch to best payoff and send it as action to neighbours and wait for actions
	switch pdAgentAwaitingNeighbourActions pdAgentNeighbourActionsReceived 


pdAgentAwaitingNeighbourActions :: SF PDAgentIn (PDAgentOut, Event ())
pdAgentAwaitingNeighbourActions = proc ain ->
	do
		let ao = agentOutFromIn ain
		let ao' = handleNeighbourAction ain ao
		let ao'' = revertAction ao'

		timeEvent <- after 1.0 () -< () -- NOTE: can also replace this by counting the number of actions received from neighbours and creating an event when all have been received
		returnA -< (ao'', timeEvent)

pdAgentNeighbourActionsReceived :: () -> PDAgentBehaviour
pdAgentNeighbourActionsReceived _ = -- TODO: send local payoff to all neighbours and wait for other payoffs
	switch pdAgentAwaitingNeighbourPayoffs pdAgentNeighbourPayoffsReceived

pdAgentBehaviour :: PDAgentBehaviour
pdAgentBehaviour = -- TODO: send current payoff to neighbours and wait for actions
	switch pdAgentAwaitingNeighbourActions pdAgentNeighbourActionsReceived

revertAction :: PDAgentOut -> PDAgentOut
revertAction ao 
	| Defector == curr = updateDomainState ao (\s -> s { pdCurrAction = Cooperator})
	| Cooperator == curr = updateDomainState ao (\s -> s { pdCurrAction = Defector})
	where
		s = aoState ao
		curr = pdCurrAction s
------------------------------------------------------------------------------------------------------------------------ 