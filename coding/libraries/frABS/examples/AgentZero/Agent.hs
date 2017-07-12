module AgentZero.Agent (
	agentZeroAgentBehaviour
  ) where

import           AgentZero.Model

import           FRP.FrABS

import           Data.Maybe

import           Control.Monad

import           Control.Monad.IfElse
import           Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-STATE functions
------------------------------------------------------------------------------------------------------------------------
isAttackingSite :: AgentZeroEnvCell -> Bool
isAttackingSite AgentZeroEnvCell{azCellState = cellState} = Attack == cellState
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
agentZeroAgentBehaviourFuncM :: Double -> AgentZeroAgentIn -> State AgentZeroAgentOut ()
agentZeroAgentBehaviourFuncM _ ain =
	do
		agentZeroRandomMoveM
		agentZeroUpdateEventCountM
		agentZeroUpdateAffectM
		agentZeroUpdateProbM
		agentZeroUpdateDispoM ain

		whenM agentZeroTakeActionM agentZeroDestroyM

agentZeroRandomMoveM :: State AgentZeroAgentOut ()
agentZeroRandomMoveM =
	do
		aid <- agentIdM
		when (aid /= 0) agentRandomMoveM

agentZeroTakeActionM :: State AgentZeroAgentOut Bool
agentZeroTakeActionM =
	do
		dispo <- domainStateFieldM azAgentDispo
		return $ dispo > 0

agentZeroDestroyM :: State AgentZeroAgentOut ()
agentZeroDestroyM =
	do
		pos <- environmentPositionM

		runEnvironmentM $
			do
				cs <- cellsAroundRadiusM pos destructionRadius
				foldM (\_ (coord, cell) -> changeCellAtM coord (cell { azCellState = Dead })) () cs

onAttackingSiteM :: State AgentZeroAgentOut Bool
onAttackingSiteM =
	do
		(_, cell) <- agentCellOnPosM
		return $ isAttackingSite cell

agentZeroUpdateEventCountM :: State AgentZeroAgentOut ()
agentZeroUpdateEventCountM =
	do
		evtCount <- domainStateFieldM azAgentEventCount

		whenM onAttackingSiteM (updateDomainStateM (\s -> s { azAgentEventCount = evtCount + 1} ))


agentZeroUpdateAffectM :: State AgentZeroAgentOut ()
agentZeroUpdateAffectM =
	do
		affect <- domainStateFieldM azAgentAffect
		learningRate <- domainStateFieldM azAgentLearningRate
		delta <- domainStateFieldM azAgentDelta
		lambda <- domainStateFieldM azAgentLambda

		attackingSite <- onAttackingSiteM

		ifThenElseM onAttackingSiteM
						(updateDomainStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))}))
						(updateDomainStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))}))

agentZeroUpdateProbM :: State AgentZeroAgentOut ()
agentZeroUpdateProbM =
	do
		pos <- environmentPositionM
		cs <- runEnvironmentM $ cellsAroundRadiusM pos sampleRadius
		mem <- domainStateFieldM azAgentMemory

		let csAttacking = filter (isAttackingSite . snd) cs
		let localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)
		let mem' = localProb : init mem
		let newProb = mean mem'

		updateDomainStateM (\s -> s { azAgentProb = newProb, azAgentMemory = mem' } )

	where
		mean :: (Fractional a) => [a] -> a
		mean xs = s / n
		    where
		        s = sum xs
		        n = fromIntegral $ length xs

agentZeroUpdateDispoM :: AgentZeroAgentIn -> State AgentZeroAgentOut ()
agentZeroUpdateDispoM ain =
	do
		aid <- agentIdM

		affect <- domainStateFieldM azAgentAffect
		prob <- domainStateFieldM azAgentProb
		thresh <- domainStateFieldM azAgentThresh
		linkIds <- runEnvironmentM $ neighbourNodesM aid

		let dispoLocal = affect + prob

		updateDomainStateM (\s -> s { azAgentDispo = dispoLocal - thresh})
		onMessageMState dispositionMessageHandleM ain
		broadcastMessageM (Disposition dispoLocal) linkIds

	where
		dispositionMessageHandleM :: AgentMessage AgentZeroMsg -> State AgentZeroAgentOut ()
		dispositionMessageHandleM (senderId, Disposition d) =
			do
				aid <- agentIdM

				mayWeight <- runEnvironmentM $ directLinkBetweenM senderId aid
				let weight = fromMaybe 0 mayWeight

				updateDomainStateM (\s -> s { azAgentDispo = azAgentDispo s + (d * weight)})

		-- NOTE: pattern match is redundant because only Disposition message exists
		-- dispositionMessageHandleM _ = return ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR NON-MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
onAttackingSite :: AgentZeroAgentOut -> Bool
onAttackingSite a = isAttackingSite cell
	where
		(_, cell) = agentCellOnPos a

agentZeroTakeAction :: AgentZeroAgentOut -> Bool
agentZeroTakeAction a = dispo > 0
	where
		dispo = azAgentDispo $ aoState a

agentZeroUpdateEventCount :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateEventCount a
	| onAttackingSite a = updateDomainState (\s -> s { azAgentEventCount = azAgentEventCount s + 1}) a 
	| otherwise = a

agentZeroUpdateAffect :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateAffect a
	| onAttackingSite a = updateDomainState (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))}) a 
	| otherwise = updateDomainState (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))}) a 

	where
		s = aoState a
		affect = azAgentAffect s
		learningRate = azAgentLearningRate s
		delta = azAgentDelta s
		lambda = azAgentLambda s

agentZeroUpdateProb :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateProb a = updateDomainState (\s -> s { azAgentProb = newProb, azAgentMemory = mem' }) a 
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius pos sampleRadius env
		csAttacking = filter (isAttackingSite . snd) cs

		localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)

		mem = azAgentMemory $ aoState a
		mem' = localProb : init mem

		newProb = mean mem'

		mean :: (Fractional a) => [a] -> a
		mean xs = s / n
		    where
		        s = sum xs
		        n = fromIntegral $ length xs

agentZeroUpdateDispo :: AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateDispo ain a = broadcastMessage (Disposition dispoLocal) linkIds aDispoFinal
	where
		aid = aoId a
		env = aoEnv a

		s = aoState a
		affect = azAgentAffect s
		prob = azAgentProb s
		thresh = azAgentThresh s
		dispoLocal = affect + prob

		linkIds = neighbourNodes aid env

		aDispoSelf = updateDomainState (\s -> s { azAgentDispo = dispoLocal - thresh}) a 
		aDispoFinal = onMessage dispositionMessageHandle ain aDispoSelf

		dispositionMessageHandle :: AgentZeroAgentOut -> AgentMessage AgentZeroMsg -> AgentZeroAgentOut
		dispositionMessageHandle a (senderId, Disposition d) = updateDomainState (\s -> s { azAgentDispo = azAgentDispo s + (d * weight)}) a
			where
				mayWeight = directLinkBetween senderId aid (aoEnv a)
				weight = fromMaybe 0 mayWeight
		-- NOTE: pattern match is redundant because only Disposition message exists
		-- dispositionMessageHandle a _ = a

agentZeroDestroy :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroDestroy a = a { aoEnv = env' }
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius pos destructionRadius env
		env' = foldr (\(coord, cell) envAcc -> changeCellAt coord (cell { azCellState = Dead }) envAcc) env cs

agentZeroRandomMove :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroRandomMove a
	| aoId a /= 0 = agentRandomMove a
	| otherwise = a

agentZeroAgentBehaviourFunc :: Double -> AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroAgentBehaviourFunc _ ain aout
	| agentZeroTakeAction agentBevoreAction = agentZeroDestroy agentBevoreAction
	| otherwise = agentBevoreAction
	where
		agentBevoreAction = agentZeroUpdateDispo ain $
								agentZeroUpdateProb $
								agentZeroUpdateAffect $
								agentZeroUpdateEventCount $
								agentZeroRandomMove aout

agentZeroAgentBehaviour :: AgentZeroAgentBehaviour
agentZeroAgentBehaviour = agentMonadic agentZeroAgentBehaviourFuncM  -- agentPure agentZeroAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------
