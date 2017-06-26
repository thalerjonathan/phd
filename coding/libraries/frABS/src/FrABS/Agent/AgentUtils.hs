module FrABS.Agent.AgentUtils (
	pickRandomNeighbourCell,
	pickRandomNeighbourCellM,

	agentRandomMove,
	agentRandomMoveM,

	agentCellOnPos,
	agentCellOnPosM
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import Control.Monad.Random
import Control.Monad.Trans.State

pickRandomNeighbourCell :: AgentOut s m ec l -> Rand StdGen (EnvCoord, ec)
pickRandomNeighbourCell a = 
	do
		let env = aoEnv a
		let pos = aoEnvPos a
		let neighbourCells = neighbours env pos
		let l = length neighbourCells 

		randIdx <- getRandomR (0, l - 1)

		return (neighbourCells !! randIdx)

pickRandomNeighbourCellM :: State (AgentOut s m ec l) (EnvCoord, ec)
pickRandomNeighbourCellM = state pickRandomNeighbourCellMAux
	where
		pickRandomNeighbourCellMAux :: AgentOut s m ec l -> ((EnvCoord, ec), AgentOut s m ec l)
		pickRandomNeighbourCellMAux ao = runAgentRandom ao (pickRandomNeighbourCell ao)

agentCellOnPos :: (AgentOut s m ec l) -> (EnvCoord, ec)
agentCellOnPos a = (agentPos, cellOfAgent)
    where
        env = aoEnv a
        agentPos = aoEnvPos a
        cellOfAgent = cellAt env agentPos

agentCellOnPosM :: State (AgentOut s m ec l) (EnvCoord, ec)
agentCellOnPosM = state agentCellOnPosMAux
    where
    	agentCellOnPosMAux :: AgentOut s m ec l -> ((EnvCoord, ec), AgentOut s m ec l)
    	agentCellOnPosMAux ao = (agentCellOnPos ao, ao)

agentRandomMove :: AgentOut s m ec l -> AgentOut s m ec l
agentRandomMove a = a' { aoEnvPos = coord }
	where
		((coord, _), a') = runAgentRandom a (pickRandomNeighbourCell a)

agentRandomMoveM :: State (AgentOut s m ec l) ()
agentRandomMoveM = state (\ao -> ((), agentRandomMove ao))