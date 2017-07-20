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
isAttackingSite AgentZeroEnvCell{ azCellState = cellState } = Attack == cellState

onAttackingSite :: AgentZeroEnvironment -> AgentZeroAgentState -> Bool
onAttackingSite e s = isAttackingSite cell
	where
		coord = azAgentCoord s
		coordPatch = agentCoordToPatchCoord e coord
		cell = cellAt coordPatch (azWorldPatches e)

agentCoordToPatchCoord :: AgentZeroEnvironment -> Continuous2DCoord -> Discrete2dCoord
agentCoordToPatchCoord e cc = cont2dTransDisc2d wp as cc
	where
		wp = azWorldPatches e
		as = azAgentSpace e

incrementEventCount :: AgentZeroAgentState -> AgentZeroAgentState
incrementEventCount s = s { azAgentEventCount = azAgentEventCount s + 1}

doesTakeAction :: AgentZeroAgentState -> Bool
doesTakeAction s = azAgentDispo s > 0
------------------------------------------------------------------------------------------------------------------------

{-
------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
agentZeroAgentBehaviourFuncM :: AgentZeroEnvironment 
								-> Double 
								-> AgentZeroAgentIn 
								-> State AgentZeroAgentOut ()
agentZeroAgentBehaviourFuncM e _ ain =
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
		coord <- domainStateFieldM azAgentCoord

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
-}

------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR NON-MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
updateEventCount :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateEventCount e s
	| onAttackingSite e s = incrementEventCount s
	| otherwise = s

updateAffect :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateAffect e s
	| onAttackingSite e s = s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect)) }
	| otherwise = s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect)) }
	where
		affect = azAgentAffect s
		learningRate = azAgentLearningRate s
		delta = azAgentDelta s
		lambda = azAgentLambda s

updateProb :: AgentZeroEnvironment -> AgentZeroAgentState -> AgentZeroAgentState
updateProb e s = s { azAgentProb = newProb, azAgentMemory = mem' }
	where
		coordPatch = agentCoordToPatchCoord e (azAgentCoord s)
		cs = cellsAroundRadius coordPatch sampleRadius (azWorldPatches e)
		csAttacking = filter (isAttackingSite . snd) cs

		localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)

		mem = azAgentMemory s
		mem' = localProb : init mem

		newProb = mean mem'

		mean :: (Fractional a) => [a] -> a
		mean xs = s / n
		    where
		        s = sum xs
		        n = fromIntegral $ length xs

updateDispo :: AgentZeroEnvironment -> AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
updateDispo e ain ao = broadcastMessage (Disposition dispoLocal) linkIds aDispoFinal
	where
		aid = aoId ao

		s = aoState ao
		affect = azAgentAffect s
		prob = azAgentProb s
		thresh = azAgentThresh s
		dispoLocal = affect + prob

		net = azAgentNetwork e
		linkIds = neighbourNodes aid net

		aDispoSelf = updateDomainState (\s -> s { azAgentDispo = dispoLocal - thresh }) ao
		aDispoFinal = onMessage dispositionMessageHandle ain aDispoSelf

		dispositionMessageHandle :: AgentMessage AgentZeroMsg -> AgentZeroAgentOut -> AgentZeroAgentOut
		dispositionMessageHandle (senderId, Disposition d) ao = updateDomainState (\s -> s { azAgentDispo = azAgentDispo s + (d * weight)}) ao
			where
				mayWeight = directLinkBetween senderId aid net
				weight = fromMaybe 0 mayWeight

destroyPatches :: Continuous2DCoord -> AgentZeroEnvironment -> AgentZeroEnvironment
destroyPatches coordCont e = e { azWorldPatches = wp' }
	where
		wp = azWorldPatches e
		aspace = azAgentSpace e

		coordDisc = cont2dTransDisc2d wp aspace coordCont

		cs = cellsAroundRadius coordDisc destructionRadius wp
		wp' = foldr (\(coord, cell) wpAcc -> changeCellAt coord (cell { azCellState = Dead }) wpAcc) wp cs

randomMove :: AgentZeroEnvironment -> AgentZeroAgentOut -> AgentZeroAgentOut
randomMove e ao 
	| aoId ao == 0 = ao 
	| otherwise = updateDomainState (\s -> s { azAgentCoord = newCoord }) ao'
	where
		coord = azAgentCoord $ aoState ao
		(newCoord, ao') = agentRandom (randomCoord coord (azAgentSpace e) movementSpeed) ao

agentZeroAgentBehaviourFunc :: AgentZeroEnvironment 
								-> Double 
								-> AgentZeroAgentIn 
								-> AgentZeroAgentOut 
								-> (AgentZeroAgentOut, AgentZeroEnvironment)
agentZeroAgentBehaviourFunc e _ ain ao
	| doesTakeAction s2 = (ao2, e')
	| otherwise = (ao2, e)
	where
		ao0 = randomMove e ao

		s0 = updateProb e $
				updateAffect e $
				updateEventCount e (aoState ao0)

		ao1 = setDomainState s0 ao0
		ao2 = updateDispo e ain ao1

		s2 = aoState ao2
		coord = azAgentCoord s2

		e' = destroyPatches coord e 

agentZeroAgentBehaviour :: AgentZeroAgentBehaviour
agentZeroAgentBehaviour = agentPure agentZeroAgentBehaviourFunc -- agentMonadic agentZeroAgentBehaviourFuncM  -- agentPure agentZeroAgentBehaviourFunc
------------------------------------------------------------------------------------------------------------------------
