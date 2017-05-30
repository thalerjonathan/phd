module FrABS.Agent.AgentUtils where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random
import Control.Monad.Random
import Control.Monad

pickRandomNeighbourCell :: AgentOut s m ec -> Rand StdGen (EnvCoord, ec)
pickRandomNeighbourCell a = 
	do
		let env = aoEnv a
		let pos = aoEnvPos a
		let neighbourCells = neighbours env pos
		let l = length neighbourCells 

		randIdx <- getRandomR (0, l - 1)

		return (neighbourCells !! randIdx)