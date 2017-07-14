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
import FRP.FrABS.Environment.Definitions

import Control.Monad.Random
import Control.Monad.Trans.State

import Data.Graph.Inductive.Graph

-------------------------------------------------------------------------------
-- NETWORK-ENVIRONMENT RELATED
-------------------------------------------------------------------------------
agentNeighbourNodesM ::  (EnvironmentNetwork e) => State (AgentOut s m e) [Node]
agentNeighbourNodesM = state (\ao -> ((agentNeighbourNodes ao), ao))

agentNeighbourNodes :: (EnvironmentNetwork e) => AgentOut s m e -> [Node]
agentNeighbourNodes ao = neighbourNodes selfNode env
    where
        env = aoEnv ao
        selfNode = aoId ao

pickRandomNeighbourNode :: (EnvironmentNetwork e) => AgentOut s m e -> Rand StdGen Node
pickRandomNeighbourNode a = 
    do
        let aid = aoId a
        let env = aoEnv a
        let nn = neighbourNodes aid env
        let l = length nn 

        randIdx <- getRandomR (0, l - 1)

        return (nn !! randIdx)

pickRandomNeighbourNodeM :: (EnvironmentNetwork e) => State (AgentOut s m e) Node
pickRandomNeighbourNodeM = state pickRandomNeighbourNodeMAux
    where
        pickRandomNeighbourNodeMAux :: AgentOut s m e -> (Node, AgentOut s m e)
        pickRandomNeighbourNodeMAux ao = runAgentRandom (pickRandomNeighbourNode ao) ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- DISCRETE 2D-ENVIRONMENT RELATED
-------------------------------------------------------------------------------
agentNeighbourCellsWithCoords :: (EnvironmentDiscrete2D e) => AgentOut s m e -> [(Discrete2DCoord, ec)]
agentNeighbourCellsWithCoords ao = neighbours pos env
    where
        env = aoEnv ao
        pos = aoEnvPos ao

agentNeighbourCells :: (EnvironmentDiscrete2D e) => AgentOut s m e -> [ec]
agentNeighbourCells ao = map snd (agentNeighbourCellsWithCoords ao)


pickRandomNeighbourCell :: (EnvironmentDiscrete2D e) => AgentOut s m e -> Rand StdGen (Discrete2DCoord, ec)
pickRandomNeighbourCell ao = 
	do
		let ncc = agentNeighbourCellsWithCoords ao
		let l = length ncc 

		randIdx <- getRandomR (0, l - 1)

		return (ncc !! randIdx)

pickRandomNeighbourCellM :: (EnvironmentDiscrete2D e) => State (AgentOut s m e) (Discrete2DCoord, ec)
pickRandomNeighbourCellM = state pickRandomNeighbourCellMAux
	where
		pickRandomNeighbourCellMAux :: AgentOut s m e -> ((Discrete2DCoord, ec), AgentOut s m e)
		pickRandomNeighbourCellMAux ao = runAgentRandom (pickRandomNeighbourCell ao) ao

agentCellOnPos :: (EnvironmentDiscrete2D e) => AgentOut s m e -> (Discrete2DCoord, ec)
agentCellOnPos a = (agentPos, cellOfAgent)
    where
        env = aoEnv a
        agentPos = aoEnvPos a
        cellOfAgent = cellAt agentPos env

agentCellOnPosM :: (EnvironmentDiscrete2D e) => State (AgentOut s m e) (Discrete2DCoord, ec)
agentCellOnPosM = state agentCellOnPosMAux
    where
    	agentCellOnPosMAux :: AgentOut s m e -> ((Discrete2DCoord, ec), AgentOut s m e)
    	agentCellOnPosMAux ao = (agentCellOnPos ao, ao)

agentRandomMove :: (EnvironmentDiscrete2D e) => AgentOut s m e -> AgentOut s m e
agentRandomMove a = a' { aoEnvPos = coord }
	where
		((coord, _), a') = runAgentRandom (pickRandomNeighbourCell a) a 

agentRandomMoveM :: (EnvironmentDiscrete2D e) => State (AgentOut s m e) ()
agentRandomMoveM = state (\ao -> ((), agentRandomMove ao))
-------------------------------------------------------------------------------