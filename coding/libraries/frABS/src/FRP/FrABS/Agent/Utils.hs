{-# LANGUAGE MultiParamTypeClasses #-}
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
import FRP.FrABS.Environment.Network
import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Continuous

import Control.Monad.Random
import Control.Monad.Trans.State

import Data.Graph.Inductive.Graph

-------------------------------------------------------------------------------
-- NETWORK-ENVIRONMENT RELATED
-------------------------------------------------------------------------------
agentNeighbourNodesM :: State (AgentOut s m (Network l)) [Node]
agentNeighbourNodesM = state (\ao -> ((agentNeighbourNodes ao), ao))

agentNeighbourNodes :: AgentOut s m (Network l) -> [Node]
agentNeighbourNodes ao = neighbourNodes selfNode env
    where
        env = aoEnv ao
        selfNode = aoId ao

pickRandomNeighbourNode :: AgentOut s m (Network l) -> Rand StdGen Node
pickRandomNeighbourNode a = 
    do
        let aid = aoId a
        let env = aoEnv a
        let nn = neighbourNodes aid env
        let l = length nn 

        randIdx <- getRandomR (0, l - 1)

        return (nn !! randIdx)

pickRandomNeighbourNodeM :: State (AgentOut s m (Network l)) Node
pickRandomNeighbourNodeM = state pickRandomNeighbourNodeMAux
    where
        pickRandomNeighbourNodeMAux :: AgentOut s m (Network l) -> (Node, AgentOut s m (Network l))
        pickRandomNeighbourNodeMAux ao = runAgentRandom (pickRandomNeighbourNode ao) ao
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- DISCRETE 2D-ENVIRONMENT RELATED
-------------------------------------------------------------------------------
agentPositionDiscrete :: AgentOut s m (Discrete2d c) -> Discrete2dCoord
agentPositionDiscrete ao = agentCoordDisc2d aid env
    where
        env = aoEnv ao
        aid = aoId ao

agentNeighbourCellsWithCoords :: AgentOut s m (Discrete2d c) -> [(Discrete2dCoord, c)]
agentNeighbourCellsWithCoords ao = neighbours pos env
    where
        env = aoEnv ao
        pos = agentPositionDiscrete ao

agentNeighbourCells :: AgentOut s m (Discrete2d c) -> [c]
agentNeighbourCells ao = map snd (agentNeighbourCellsWithCoords ao)

pickRandomNeighbourCell :: AgentOut s m (Discrete2d c) -> Rand StdGen (Discrete2dCoord, c)
pickRandomNeighbourCell ao = 
	do
		let ncc = agentNeighbourCellsWithCoords ao
		let l = length ncc 

		randIdx <- getRandomR (0, l - 1)

		return (ncc !! randIdx)

pickRandomNeighbourCellM :: State (AgentOut s m (Discrete2d c)) (Discrete2dCoord, c)
pickRandomNeighbourCellM = state pickRandomNeighbourCellMAux
	where
		pickRandomNeighbourCellMAux :: AgentOut s m (Discrete2d c) -> ((Discrete2dCoord, c), AgentOut s m (Discrete2d c))
		pickRandomNeighbourCellMAux ao = runAgentRandom (pickRandomNeighbourCell ao) ao

agentCellOnPos :: AgentOut s m (Discrete2d c) -> (Discrete2dCoord, c)
agentCellOnPos ao = (agentPos, cellOfAgent)
    where
        env = aoEnv ao
        agentPos = agentPositionDiscrete ao
        cellOfAgent = cellAt agentPos env

agentCellOnPosM :: State (AgentOut s m (Discrete2d c)) (Discrete2dCoord, c)
agentCellOnPosM = state agentCellOnPosMAux
    where
    	agentCellOnPosMAux :: AgentOut s m (Discrete2d c) -> ((Discrete2dCoord, c), AgentOut s m (Discrete2d c))
    	agentCellOnPosMAux ao = (agentCellOnPos ao, ao)

agentRandomMove :: AgentOut s m (Discrete2d c) -> AgentOut s m (Discrete2d c)
agentRandomMove ao = ao' { aoEnv = env' }
	where
        ((coord, _), ao') = runAgentRandom (pickRandomNeighbourCell ao) ao

        aid = aoId ao
        env = aoEnv ao
        env' = updateAgentCoordDisc2d aid coord env
	
agentRandomMoveM :: State (AgentOut s m (Discrete2d c)) ()
agentRandomMoveM = state (\ao -> ((), agentRandomMove ao))
-------------------------------------------------------------------------------