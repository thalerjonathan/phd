module FRP.FrABS.Agent.Utils (
    agentNeighbourNodes,
    agentNeighbourNodesM,
    agentNeighbourCells,
    agentNeighbourCellsWithCoords,

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
import FRP.FrABS.Environment.Discrete

import Control.Monad.Random
import Control.Monad.Trans.State

import Data.Graph.Inductive.Graph

agentNeighbourNodesM ::  State (AgentOut s m ec l) [Node]
agentNeighbourNodesM = state (\ao -> ((agentNeighbourNodes ao), ao))

agentNeighbourNodes :: AgentOut s m ec l -> [Node]
agentNeighbourNodes ao = neighbourNodes selfNode env
    where
        env = aoEnv ao
        selfNode = aoId ao

agentNeighbourCells :: AgentOut s m ec l -> [ec]
agentNeighbourCells ao = map snd (agentNeighbourCellsWithCoords ao)

agentNeighbourCellsWithCoords :: AgentOut s m ec l -> [(EnvCoord, ec)]
agentNeighbourCellsWithCoords ao = neighbours pos env
    where
        env = aoEnv ao
        pos = aoEnvPos ao

pickRandomNeighbourNode :: AgentOut s m ec l -> Rand StdGen Node
pickRandomNeighbourNode a = 
    do
        let aid = aoId a
        let env = aoEnv a
        let nn = neighbourNodes aid env
        let l = length nn 

        randIdx <- getRandomR (0, l - 1)

        return (nn !! randIdx)

pickRandomNeighbourNodeM :: State (AgentOut s m ec l) Node
pickRandomNeighbourNodeM = state pickRandomNeighbourNodeMAux
    where
        pickRandomNeighbourNodeMAux :: AgentOut s m ec l -> (Node, AgentOut s m ec l)
        pickRandomNeighbourNodeMAux ao = runAgentRandom (pickRandomNeighbourNode ao) ao

pickRandomNeighbourCell :: AgentOut s m ec l -> Rand StdGen (EnvCoord, ec)
pickRandomNeighbourCell ao = 
	do
		let ncc = agentNeighbourCellsWithCoords ao
		let l = length ncc 

		randIdx <- getRandomR (0, l - 1)

		return (ncc !! randIdx)

pickRandomNeighbourCellM :: State (AgentOut s m ec l) (EnvCoord, ec)
pickRandomNeighbourCellM = state pickRandomNeighbourCellMAux
	where
		pickRandomNeighbourCellMAux :: AgentOut s m ec l -> ((EnvCoord, ec), AgentOut s m ec l)
		pickRandomNeighbourCellMAux ao = runAgentRandom (pickRandomNeighbourCell ao) ao

agentCellOnPos :: AgentOut s m ec l -> (EnvCoord, ec)
agentCellOnPos a = (agentPos, cellOfAgent)
    where
        env = aoEnv a
        agentPos = aoEnvPos a
        cellOfAgent = cellAt agentPos env

agentCellOnPosM :: State (AgentOut s m ec l) (EnvCoord, ec)
agentCellOnPosM = state agentCellOnPosMAux
    where
    	agentCellOnPosMAux :: AgentOut s m ec l -> ((EnvCoord, ec), AgentOut s m ec l)
    	agentCellOnPosMAux ao = (agentCellOnPos ao, ao)

agentRandomMove :: AgentOut s m ec l -> AgentOut s m ec l
agentRandomMove a = a' { aoEnvPos = coord }
	where
		((coord, _), a') = runAgentRandom (pickRandomNeighbourCell a) a 

agentRandomMoveM :: State (AgentOut s m ec l) ()
agentRandomMoveM = state (\ao -> ((), agentRandomMove ao))