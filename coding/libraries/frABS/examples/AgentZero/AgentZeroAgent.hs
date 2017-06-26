{-# LANGUAGE Arrows #-}
module AgentZero.AgentZeroAgent where

-- Project-internal import first
import AgentZero.AgentZeroModel
import AgentZero.AgentZeroEnvironment

import FrABS.Env.Environment
import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Data.Map as Map

-- debugging imports finally, to be easily removed in final version
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

		takeAction <- agentZeroTakeActionM
		if takeAction then
			agentZeroDestroyM
			else
				return ()

agentZeroRandomMoveM :: State AgentZeroAgentOut ()
agentZeroRandomMoveM =
	do
		aid <- agentIdM
		if aid /= 0 then
			agentRandomMoveM
			else
				return ()

agentZeroTakeActionM :: State AgentZeroAgentOut Bool
agentZeroTakeActionM =
	do
		dispo <- domainStateFieldM azAgentDispo
		return $ dispo > 0

agentZeroDestroyM :: State AgentZeroAgentOut ()
agentZeroDestroyM = 
	do
		env <- environmentM
		pos <- environmentPositionM
		-- TODO: need better environment access and change
		let cs = cellsAroundRadius env pos destructionRadius
		let env' = foldr (\(coord, cell) envAcc -> changeCellAt envAcc coord (cell { azCellState = Dead })) env cs
		ao <- get
		put $ ao { aoEnv = env' }

onAttackingSiteM :: State AgentZeroAgentOut Bool
onAttackingSiteM =
	do
		(_, cell) <- agentCellOnPosM
		return $ isAttackingSite cell

agentZeroUpdateEventCountM :: State AgentZeroAgentOut ()
agentZeroUpdateEventCountM =
	do
		attackingSite <- onAttackingSiteM
		evtCount <- domainStateFieldM azAgentEventCount
		if attackingSite then
			updateDomainStateM (\s -> s { azAgentEventCount = evtCount + 1} )
			else
				return ()

agentZeroUpdateAffectM :: State AgentZeroAgentOut ()
agentZeroUpdateAffectM =
	do
		affect <- domainStateFieldM azAgentAffect
		learningRate <- domainStateFieldM azAgentLearningRate
		delta <- domainStateFieldM azAgentDelta
		lambda <- domainStateFieldM azAgentLambda

		attackingSite <- onAttackingSiteM
		if attackingSite then
			updateDomainStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))})
			else
				updateDomainStateM (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))})


agentZeroUpdateProbM :: State AgentZeroAgentOut ()
agentZeroUpdateProbM =
	do
		env <- environmentM
		pos <- environmentPositionM

		let cs = cellsAroundRadius env pos sampleRadius
		let csAttacking = filter (isAttackingSite . snd) cs
		let localProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)

		mem <- domainStateFieldM azAgentMemory

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
		env <- environmentM

		affect <- domainStateFieldM azAgentAffect
		prob <- domainStateFieldM azAgentProb
		thresh <- domainStateFieldM azAgentThresh
		
		let dispoLocal = affect + prob

		let linkIds = neighbourNodes env aid

		updateDomainStateM (\s -> s { azAgentDispo = dispoLocal - thresh})

		-- TODO: rework this onMessage, not very nice
		ao <- get
		put $ onMessage ain dispositionMessageHandle ao

		broadcastMessageM (Disposition dispoLocal) linkIds

	where
		dispositionMessageHandle :: AgentZeroAgentOut -> AgentMessage AgentZeroMsg -> AgentZeroAgentOut
		dispositionMessageHandle a (senderId, (Disposition d)) = updateDomainState a (\s -> s { azAgentDispo = (azAgentDispo s) + (d * weight)})
			where
				mayWeight = directLinkBetween (aoEnv a) senderId (aoId a)
				weight = maybe 0.0 Prelude.id mayWeight
		dispositionMessageHandle a _ = a
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
		dispositionMessageHandle a _ = a

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