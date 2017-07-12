module FRP.FrABS.Agent.Utils (
    neighbourCells,
    neighbourCellsWithCoords,

    pickRandomNeighbourNode,
    pickRandomNeighbourNodeM,
    
    pickRandomNeighbourCell,
	pickRandomNeighbourCellM,

	agentRandomMove,
	agentRandomMoveM,

	agentCellOnPos,
	agentCellOnPosM
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Random
import FRP.FrABS.Env.Environment

import Control.Monad.Random
import Control.Monad.Trans.State

import Data.Graph.Inductive.Graph

neighbourCells :: AgentOut s m ec l -> [ec]
neighbourCells ao = map snd (neighbourCellsWithCoords ao)

neighbourCellsWithCoords :: AgentOut s m ec l -> [(EnvCoord, ec)]
neighbourCellsWithCoords ao = neighbours env pos
    where
        env = aoEnv ao
        pos = aoEnvPos ao

pickRandomNeighbourNode :: AgentOut s m ec l -> Rand StdGen Node
pickRandomNeighbourNode a = 
    do
        let aid = aoId a
        let env = aoEnv a
        let nn = neighbourNodes env aid
        let l = length nn 

        randIdx <- getRandomR (0, l - 1)

        return (nn !! randIdx)

pickRandomNeighbourNodeM :: State (AgentOut s m ec l) Node
pickRandomNeighbourNodeM = state pickRandomNeighbourNodeMAux
    where
        pickRandomNeighbourNodeMAux :: AgentOut s m ec l -> (Node, AgentOut s m ec l)
        pickRandomNeighbourNodeMAux ao = runAgentRandom ao (pickRandomNeighbourNode ao)

pickRandomNeighbourCell :: AgentOut s m ec l -> Rand StdGen (EnvCoord, ec)
pickRandomNeighbourCell ao = 
	do
		let ncc = neighbourCellsWithCoords ao
		let l = length ncc 

		randIdx <- getRandomR (0, l - 1)

		return (ncc !! randIdx)

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