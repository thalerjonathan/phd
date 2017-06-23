module FrABS.Agent.AgentUtils where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

import Control.Monad
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