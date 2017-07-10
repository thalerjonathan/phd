{-# LANGUAGE Arrows #-}
module AgentZero.AgentZeroAgent where

import AgentZero.AgentZeroModel
import AgentZero.AgentZeroEnvironment

import FrABS.Env.Environment
import FrABS.Agent.Agent
import FrABS.Agent.Utils
import FrABS.Agent.Monad

import FRP.Yampa

import Data.Maybe
import Data.List
import System.Random
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.State
import Control.Monad.IfElse
import qualified Data.Map as Map

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-STATE functions
------------------------------------------------------------------------------------------------------------------------
isAttackingSite :: AgentZeroEnvCell -> Bool
isAttackingSite AgentZeroEnvCell{azCellState = cellState} = Attack == cellState
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
agentZeroAgentBehaviourFuncM :: AgentZeroAgentIn -> State AgentZeroAgentOut ()
agentZeroAgentBehaviourFuncM ain = 
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
		let mem' = localProb : (init mem)
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
		onMessageMState ain dispositionMessageHandleM
		broadcastMessageM (Disposition dispoLocal) linkIds

	where
		dispositionMessageHandleM :: AgentMessage AgentZeroMsg -> State AgentZeroAgentOut ()
		dispositionMessageHandleM (senderId, (Disposition d)) = 
			do
				aid <- agentIdM

				mayWeight <- runEnvironmentM $ directLinkBetweenM senderId aid
				let weight = maybe 0.0 Prelude.id mayWeight

				updateDomainStateM (\s -> s { azAgentDispo = (azAgentDispo s) + (d * weight)})

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
	| onAttackingSite a = updateDomainState a (\s -> s { azAgentEventCount = (azAgentEventCount s) + 1} )
	| otherwise = a

agentZeroUpdateAffect :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateAffect a 
	| onAttackingSite a = updateDomainState a (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))})
	| otherwise = updateDomainState a (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))})

	where
		s = aoState a 
		affect = azAgentAffect s
		learningRate = azAgentLearningRate s
		delta = azAgentDelta s
		lambda = azAgentLambda s

agentZeroUpdateProb :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateProb a = updateDomainState a (\s -> s { azAgentProb = newProb, azAgentMemory = mem' } )
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius env pos sampleRadius
		csAttacking = filter (isAttackingSite . snd) cs

		localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)

		mem = azAgentMemory $ aoState a
		mem' = localProb : (init mem)

		newProb = mean mem'

		mean :: (Fractional a) => [a] -> a
		mean xs = s / n
		    where
		        s = sum xs
		        n = fromIntegral $ length xs

agentZeroUpdateDispo :: AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateDispo ain a = broadcastMessage aDispoFinal (Disposition dispoLocal) linkIds
	where
		aid = aoId a
		env = aoEnv a 

		s = aoState a
		affect = azAgentAffect s
		prob = azAgentProb s
		thresh = azAgentThresh s 
		dispoLocal = affect + prob

		linkIds = neighbourNodes env aid

		aDispoSelf = updateDomainState a (\s -> s { azAgentDispo = dispoLocal - thresh})
		aDispoFinal = onMessage ain dispositionMessageHandle aDispoSelf

		dispositionMessageHandle :: AgentZeroAgentOut -> AgentMessage AgentZeroMsg -> AgentZeroAgentOut
		dispositionMessageHandle a (senderId, (Disposition d)) = updateDomainState a (\s -> s { azAgentDispo = (azAgentDispo s) + (d * weight)})
			where
				mayWeight = directLinkBetween (aoEnv a) senderId aid
				weight = maybe 0.0 id mayWeight
		-- NOTE: pattern match is redundant because only Disposition message exists
		-- dispositionMessageHandle a _ = a

agentZeroDestroy :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroDestroy a = a { aoEnv = env' }
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius env pos destructionRadius
		env' = foldr (\(coord, cell) envAcc -> changeCellAt envAcc coord (cell { azCellState = Dead })) env cs

agentZeroRandomMove :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroRandomMove a 
	| aoId a /= 0 = agentRandomMove a
	| otherwise = a

agentZeroAgentBehaviourFunc :: AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut 
agentZeroAgentBehaviourFunc ain aout 
	| agentZeroTakeAction agentBevoreAction = agentZeroDestroy agentBevoreAction
	| otherwise = agentBevoreAction
	where
		agentBevoreAction = (agentZeroUpdateDispo ain) $ 
								agentZeroUpdateProb $ 
								agentZeroUpdateAffect $ 
								agentZeroUpdateEventCount $ 
								agentZeroRandomMove aout

agentZeroAgentBehaviour :: AgentZeroAgentBehaviour
agentZeroAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        -- let ao' = agentZeroAgentBehaviourFunc ain ao
        let ao' = execState (agentZeroAgentBehaviourFuncM ain) ao
        returnA -< ao'
------------------------------------------------------------------------------------------------------------------------