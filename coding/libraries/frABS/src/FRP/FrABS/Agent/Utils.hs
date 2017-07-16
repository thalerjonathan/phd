module FRP.FrABS.Agent.Utils (
    agentPositionDiscrete,

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
agentNeighbourNodesM ::  (EnvNet e) => State (AgentOut s m e) [Node]
agentNeighbourNodesM = state (\ao -> ((agentNeighbourNodes ao), ao))

agentNeighbourNodes :: (EnvNet e) => AgentOut s m e -> [Node]
agentNeighbourNodes ao = neighbourNodes selfNode env
    where
        env = aoEnv ao
        selfNode = aoId ao

pickRandomNeighbourNode :: (EnvNet e) => AgentOut s m e -> Rand StdGen Node
pickRandomNeighbourNode a = 
    do
        let aid = aoId a
        let env = aoEnv a
        let nn = neighbourNodes aid env
        let l = length nn 

        randIdx <- getRandomR (0, l - 1)

        return (nn !! randIdx)

pickRandomNeighbourNodeM :: (EnvNet e) => State (AgentOut s m e) Node
pickRandomNeighbourNodeM = state pickRandomNeighbourNodeMAux
    where
        pickRandomNeighbourNodeMAux :: (EnvNet e) => AgentOut s m e -> (Node, AgentOut s m e)
        pickRandomNeighbourNodeMAux ao = runAgentRandom (pickRandomNeighbourNode ao) ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- DISCRETE 2D-ENVIRONMENT RELATED
-------------------------------------------------------------------------------
agentPositionDiscrete :: (EnvDisc2d e) => AgentOut s m e -> Discrete2DCoord
agentPositionDiscrete ao = agentCoordDisc2D aid env
    where
        env = aoEnv ao
        aid = aoId ao

agentNeighbourCellsWithCoords :: (EnvDisc2d e) => AgentOut s m e -> [(Discrete2DCoord, ec)]
agentNeighbourCellsWithCoords ao = neighbours pos env
    where
        env = aoEnv ao
        pos = agentPositionDiscrete ao

agentNeighbourCells :: (EnvDisc2d e) => AgentOut s m e -> [ec]
agentNeighbourCells ao = map snd (agentNeighbourCellsWithCoords ao)


pickRandomNeighbourCell :: (EnvDisc2d e) => AgentOut s m e -> Rand StdGen (Discrete2DCoord, ec)
pickRandomNeighbourCell ao = 
	do
		let ncc = agentNeighbourCellsWithCoords ao
		let l = length ncc 

		randIdx <- getRandomR (0, l - 1)

		return (ncc !! randIdx)

pickRandomNeighbourCellM :: (EnvDisc2d e) => State (AgentOut s m e) (Discrete2DCoord, ec)
pickRandomNeighbourCellM = state pickRandomNeighbourCellMAux
	where
		pickRandomNeighbourCellMAux :: (EnvDisc2d e) => AgentOut s m e -> ((Discrete2DCoord, ec), AgentOut s m e)
		pickRandomNeighbourCellMAux ao = runAgentRandom (pickRandomNeighbourCell ao) ao

agentCellOnPos :: (EnvDisc2d e) => AgentOut s m e -> (Discrete2DCoord, ec)
agentCellOnPos ao = (agentPos, cellOfAgent)
    where
        env = aoEnv ao
        agentPos = agentPositionDiscrete ao
        cellOfAgent = cellAt agentPos env

agentCellOnPosM :: (EnvDisc2d e) => State (AgentOut s m e) (Discrete2DCoord, ec)
agentCellOnPosM = state agentCellOnPosMAux
    where
    	agentCellOnPosMAux :: (EnvDisc2d e) => AgentOut s m e -> ((Discrete2DCoord, ec), AgentOut s m e)
    	agentCellOnPosMAux ao = (agentCellOnPos ao, ao)

agentRandomMove :: (EnvDisc2d e) => AgentOut s m e -> AgentOut s m e
agentRandomMove ao = ao' { aoEnv = env' }
	where
        ((coord, _), ao') = runAgentRandom (pickRandomNeighbourCell ao) ao

        aid = aoId ao
        env = aoEnv ao
        env' = updateAgentCoordDisc2D aid coord env
	
agentRandomMoveM :: (EnvDisc2d e) => State (AgentOut s m e) ()
agentRandomMoveM = state (\ao -> ((), agentRandomMove ao))
-------------------------------------------------------------------------------