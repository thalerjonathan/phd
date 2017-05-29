{-# LANGUAGE Arrows #-}
module AgentZero.AgentZeroAgent where

-- Project-internal import first
import AgentZero.AgentZeroModel
import AgentZero.AgentZeroEnvironment
import FrABS.Env.Environment
import FrABS.Agent.Agent

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random
import Control.Monad

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
isAttackingSite :: AgentZeroEnvCell -> Bool
isAttackingSite AgentZeroEnvCell{azCellState = cellState} = Attack == cellState

onAttackingSite :: AgentZeroAgentOut -> Bool
onAttackingSite a = isAttackingSite cell
	where
		(_, cell) = agentZeroCellOnPos a

agentZeroTakeAction :: AgentZeroAgentOut -> Bool
agentZeroTakeAction a = dispo > 0
	where
		dispo = azAgentDispo $ aoState a

agentZeroCellOnPos :: AgentZeroAgentOut -> (EnvCoord, AgentZeroEnvCell)
agentZeroCellOnPos a = (agentPos, cellOfAgent)
    where
        env = aoEnv a
        agentPos = aoEnvPos a
        cellOfAgent = cellAt env agentPos


agentZeroUpdateEventCount :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateEventCount a 
	| onAttackingSite a = updateState a (\s -> s { azAgentEventCount = (azAgentEventCount s) + 1} )
	| otherwise = a

agentZeroUpdateAffect :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateAffect a 
	| onAttackingSite a = updateState a (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * (lambda - affect))})
	| otherwise = updateState a (\s -> s { azAgentAffect = affect + (learningRate * (affect ** delta) * extinctionRate * (0 - affect))})

	where
		state = aoState a 
		affect = azAgentAffect state
		learningRate = azAgentLearningRate state
		delta = azAgentDelta state
		lambda = azAgentLambda state

agentZeroUpdateProb :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateProb a = updateState a (\s -> s { azAgentProb = currentProb } )
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius env pos sampleRadius
		csAttacking = filter (isAttackingSite . snd) cs

		currentProb = fromRational (fromIntegral $ length csAttacking) / (fromIntegral $ length cs)
		-- TODO: add to memory and calculate median


agentZeroUpdateDispo :: AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateDispo ain a = aDispoOthers -- TODO: send dispoLocal to graph-neighbours 
	where
		s = aoState a
		affect = azAgentAffect s
		prob = azAgentProb s
		thresh = azAgentThresh s 
		dispoLocal = affect + prob

		aDispoSelf = updateState a (\s -> s { azAgentDispo = dispoLocal - thresh})
		aDispoOthers = onMessage dispositionMessageFilter ain dispositionMessageHandle a

		dispositionMessageFilter :: AgentMessage AgentZeroMsg -> Bool
		dispositionMessageFilter (_, (Disposition _)) = True
		dispositionMessageFilter _ = False

		dispositionMessageHandle :: AgentZeroAgentOut -> AgentMessage AgentZeroMsg -> AgentZeroAgentOut
		dispositionMessageHandle a (senderId, (Disposition d)) = updateState a (\s -> s { azAgentDispo = (azAgentDispo s) + (d * weight)})
			where
				weight = 1.0 -- TODO: get weight by senderId

agentZeroDestroy :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroDestroy a = a { aoEnv = env' }
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius env pos destructionRadius
		env' = foldr (\(coord, cell) envAcc -> changeCellAt envAcc coord (cell { azCellState = Dead })) env cs

agentZeroRandomMove :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroRandomMove a = a' { aoEnvPos = coord }
	where
		((coord, _), a') = runAgentRandom a (pickRandomNeighbourCell a)

		pickRandomNeighbourCell :: AgentZeroAgentOut -> Rand StdGen (EnvCoord, AgentZeroEnvCell)
		pickRandomNeighbourCell a = 
			do
				let env = aoEnv a
				let pos = aoEnvPos a
				let neighbourCells = neighbours env pos
				let l = length neighbourCells 

				randIdx <- getRandomR (0, l - 1)

				return (neighbourCells !! randIdx)

agentZeroAgentBehaviourFunc :: Double -> AgentZeroAgentIn -> AgentZeroAgentOut -> AgentZeroAgentOut 
agentZeroAgentBehaviourFunc age ain aout = agentZeroDestroy $ agentZeroRandomMove aout

agentZeroAgentBehaviour :: AgentZeroAgentBehaviour
agentZeroAgentBehaviour = proc ain ->
    do
        age <- time -< 0

        let aout = agentOutFromIn ain
        returnA -< agentZeroAgentBehaviourFunc age ain aout
------------------------------------------------------------------------------------------------------------------------