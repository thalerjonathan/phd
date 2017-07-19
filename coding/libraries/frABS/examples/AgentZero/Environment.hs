{-# LANGUAGE Arrows #-}
module AgentZero.Environment (
	AgentZeroEnvironmentCollapsing,

	agentZeroEnvironmentsCollapse,
  	agentZeroEnvironmentBehaviour
  ) where

import           AgentZero.Model

import           FRP.FrABS

import           Control.Monad.Random
import           FRP.Yampa

import           Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-COLLAPSING (parallel strategy)
------------------------------------------------------------------------------------------------------------------------
type AgentZeroEnvironmentCollapsing = EnvironmentCollapsing AgentZeroEnvironment

agentZeroEnvironmentsCollapse :: AgentZeroEnvironmentCollapsing
agentZeroEnvironmentsCollapse envs = initEnv { azWorldPatches = wpc }
	where
		initEnv = head envs

		wpes = map azWorldPatches envs
		wpc = agentZeroEnvironmentsCollapse' wpes

agentZeroEnvironmentsCollapse' :: [AgentZeroWorldPatches] -> AgentZeroWorldPatches
agentZeroEnvironmentsCollapse' envs = foldr mergeEnvs initEnv envs
	where
		initEnv = head envs

		mergeEnvs :: AgentZeroWorldPatches -> AgentZeroWorldPatches -> AgentZeroWorldPatches
		mergeEnvs env envAcc = foldr (\((coord, cell), (coordAcc, cellAcc)) acc -> changeCellAt coordAcc (mergeCells cell cellAcc) acc) envAcc zippedCells
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
-- ENVIRONMENT-BEHAVIOUR NON-MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
randomAttack :: AgentZeroWorldPatches -> Rand StdGen AgentZeroWorldPatches
randomAttack env =
	do
		let allCells = allCellsWithCoords env
		let cellCount = length allCells
		randActivationsInf <- getRandoms
		let randActivations = take cellCount randActivationsInf
		let allCells' = zipWith randAttackCell allCells randActivations
		let env' = foldr (\(coord, cell) envAcc -> changeCellAt coord cell envAcc) env allCells'
		return env'

randAttackCell :: (Discrete2dCoord, AgentZeroEnvCell) -> Double -> (Discrete2dCoord, AgentZeroEnvCell)
randAttackCell cc@(coord, cell) rand
	| x >= 12 && y >= 15 = (coord, cell { azCellState = state' })
	| otherwise = cc
	where
		(x,y) = coord
		state = azCellState cell
		state' = selectNewState state rand

		selectNewState :: AgentZeroCellState -> Double -> AgentZeroCellState
		selectNewState Dead _ = Dead
		selectNewState Friendly rand = if rand > 0.8 then Attack else Friendly
		selectNewState Attack rand = if rand > 0.5 then Friendly else Attack

randAttackCell' :: ((Discrete2dCoord, AgentZeroEnvCell), Double) -> (Discrete2dCoord, AgentZeroEnvCell)
randAttackCell' (cc@(coord, cell), rand)
	| x >= 12 && y >= 15 = (coord, cell { azCellState = state' })
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
randAttackCellSF :: StdGen -> SF (Discrete2dCoord, AgentZeroEnvCell) (Discrete2dCoord, AgentZeroEnvCell)
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
agentZeroEnvironmentBehaviour = proc e ->
    do
        t <- time -< 0

        let wp = azWorldPatches e
        let g = envDisc2dRng wp

        let (wp', g') = runRand (randomAttack wp) g
        let wp'' = wp' { envDisc2dRng = g' }

        let e' = e { azWorldPatches = wp'' }
        returnA -< trace ("Time = " ++ show t) e'
