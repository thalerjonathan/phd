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
import Control.Monad.Random
import Control.Monad
import qualified Data.Map as Map

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
		s = aoState a 
		affect = azAgentAffect s
		learningRate = azAgentLearningRate s
		delta = azAgentDelta s
		lambda = azAgentLambda s

agentZeroUpdateProb :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroUpdateProb a = updateState a (\s -> s { azAgentProb = newProb, azAgentMemory = mem' } )
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
agentZeroUpdateDispo ain a = broadcastMessage aDispoFinal (Disposition dispoLocal) connIds
	where
		s = aoState a

		affect = azAgentAffect s
		prob = azAgentProb s
		thresh = azAgentThresh s 
		dispoLocal = affect + prob

		connIds = Map.keys $ azAgentConnections s

		aDispoSelf = updateState a (\s -> s { azAgentDispo = dispoLocal - thresh})
		aDispoFinal = onMessage dispositionMessageFilter ain dispositionMessageHandle aDispoSelf

		dispositionMessageFilter :: AgentMessage AgentZeroMsg -> Bool
		dispositionMessageFilter (_, (Disposition _)) = True
		dispositionMessageFilter _ = False

		dispositionMessageHandle :: AgentZeroAgentOut -> AgentMessage AgentZeroMsg -> AgentZeroAgentOut
		dispositionMessageHandle a (senderId, (Disposition d)) = updateState a (\s -> s { azAgentDispo = (azAgentDispo s) + (d * weight)})
			where
				s = aoState a
				conns = azAgentConnections s
				weight = conns Map.! senderId

agentZeroDestroy :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroDestroy a = a { aoEnv = env' }
	where
		env = aoEnv a
		pos = aoEnvPos a
		cs = cellsAroundRadius env pos destructionRadius
		env' = foldr (\(coord, cell) envAcc -> changeCellAt envAcc coord (cell { azCellState = Dead })) env cs

agentZeroRandomMove :: AgentZeroAgentOut -> AgentZeroAgentOut
agentZeroRandomMove a 
	| aoId a /= 0 = a' { aoEnvPos = coord }
	| otherwise = a
	where
		((coord, _), a') = runAgentRandom a (pickRandomNeighbourCell a)

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
        let aout = agentOutFromIn ain
        returnA -< agentZeroAgentBehaviourFunc ain aout
------------------------------------------------------------------------------------------------------------------------