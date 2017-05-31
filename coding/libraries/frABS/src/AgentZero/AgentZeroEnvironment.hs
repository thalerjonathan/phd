{-# LANGUAGE Arrows #-}
module AgentZero.AgentZeroEnvironment where

-- Project-internal import first
import AgentZero.AgentZeroModel
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa
import Data.Maybe
import System.Random
import Control.Monad.Random

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-COLLAPSING (parallel strategy)
------------------------------------------------------------------------------------------------------------------------
type AgentZeroEnvironmentCollapsing = EnvironmentCollapsing AgentZeroEnvCell Double

agentZeroEnvironmentsCollapse :: AgentZeroEnvironmentCollapsing
agentZeroEnvironmentsCollapse envs = foldr mergeEnvs initEnv envs
	where
		initEnv = head envs

		mergeEnvs :: AgentZeroEnvironment -> AgentZeroEnvironment -> AgentZeroEnvironment
		mergeEnvs env envAcc = foldr (\((coord, cell), (coordAcc, cellAcc)) acc -> changeCellAt acc coordAcc (mergeCells cell cellAcc)) envAcc zippedCells
			where
				envCells = allCellsWithCoords env 
				envAccCells = allCellsWithCoords envAcc
				zippedCells = zip envCells envAccCells

		-- NOTE: agents only destroy, which must be merged - all other states are the same in both environments
		mergeCells :: AgentZeroEnvCell -> AgentZeroEnvCell -> AgentZeroEnvCell
		mergeCells cellA cellB 
			| Dead == cellStateA = cellA
			| Dead == cellStateB = cellB
			| otherwise = cellA
			where
				cellStateA = azCellState cellA
				cellStateB = azCellState cellB
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
randomAttack :: AgentZeroEnvironment -> Rand StdGen AgentZeroEnvironment 
randomAttack env = 
	do
		let allCells = allCellsWithCoords env
		let cellCount = length allCells
		randActivationsInf <- getRandoms
		let randActivations = take cellCount randActivationsInf
		let allCells' = map randAttackCell (zip allCells randActivations)
		let env' = foldr (\(coord, cell) envAcc -> changeCellAt envAcc coord cell) env allCells'
		return env'

randAttackCell :: ((EnvCoord, AgentZeroEnvCell), Double) -> (EnvCoord, AgentZeroEnvCell)
randAttackCell (cc@(coord, cell), rand) 
	| (x >= 12 && y >= 15) = (coord, cell { azCellState = state' })
	| otherwise = cc
	where
		(x,y) = coord
		state = azCellState cell
		state' = selectNewState state rand

		selectNewState :: AgentZeroCellState -> Double -> AgentZeroCellState
		selectNewState Dead _ = Dead
		selectNewState Friendly rand = if rand > 0.8 then Attack else Friendly
		selectNewState Attack rand = if rand > 0.5 then Friendly else Attack
			
{- TODO: when switching to arrowized programming
randAttackCellSF :: StdGen -> SF (EnvCoord, AgentZeroEnvCell) (EnvCoord, AgentZeroEnvCell)
randAttackCellSF g = proc cc ->
	do
		let (coord, cell) = cc
		randAttack <- noise g -< ()  -- TODO: use occasionally !
		let b = randAttack :: Bool
		let state = if randAttack then
						Attack
						else
							Friendly
		returnA -< (coord, cell {azCellState = state})
-}



agentZeroEnvironmentBehaviour :: AgentZeroEnvironmentBehaviour
agentZeroEnvironmentBehaviour = proc env ->
    do
        t <- time -< 0
        let g = envRng env
        let (env', g') = runRand (randomAttack env) g
        let env'' = env' { envRng = g' }
        returnA -< trace ("Time = " ++ (show t)) env''
