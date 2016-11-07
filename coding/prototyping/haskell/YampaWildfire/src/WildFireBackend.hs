{-# LANGUAGE Arrows #-}

module WildFireBackend where

import FRP.Yampa
import FRP.Yampa.Switches

data CellStatus = LIVING | BURNING | DEAD deriving (Eq)
type CellCoord = (Int, Int)

data CellState = CellState {
    csFuel :: Double,
    csStatus :: CellStatus,
    csCoord :: CellCoord
}

data CellInput = CellInput {
    ciIgnite :: Bool
}

data CellOutput = CellOutput {
    coState :: CellState,
    coBurning :: Event (),
    coDead :: Event (),
    coIgniteNeighbours :: [CellCoord]
}

instance Eq CellState where
    c1 == c2 = csCoord c1 == csCoord c2

type Cell = SF CellInput CellOutput

data SimulationIn = SimulationIn {
    ignitionIn :: Maybe CellCoord
}

data SimulationOut = SimulationOut {
    cellsOut :: [CellState]
}

{-
cell :: CellState -> Cell
cell cs = proc ci ->
    do
        let igniteCell = ciIgnite ci
        let fuel = csFuel cs
        burning <- edge -< igniteCell
        dead <- edge -< fuel <= 0.0
        returnA -< CellOutput {
            coBurning = burning,
            coDead = dead,
            coState = cs }

process' :: [CellState] -> SF SimulationIn SimulationOut
process' cells = dpSwitch route cells

route :: (SimulationIn, CellState) -> [sf] -> [(CellInput, sf)]
route (si, cs) cells = [] -- TODO: if ignition-coord == cell-coord then send cell the ignition event
-}

process :: [CellState] -> SF SimulationIn SimulationOut
process initCells = proc simIn ->
    do
        simOut <- par route (cellStatesToSF initCells) -< simIn
        returnA -< SimulationOut { cellsOut = simOut }

cellStatesToSF :: [CellState] -> [SF SimulationIn CellState]
cellStatesToSF cells = map (\c -> switch (cellLiving c) cellBurningSwitch) cells

route :: SimulationIn -> [sf] -> [(SimulationIn, sf)]
route simIn cells = map (\c -> (simIn, c) ) cells

cellLiving :: CellState -> SF SimulationIn (CellState, Event CellState)
cellLiving cell = proc simIn ->
    do
        let coord = csCoord cell
        let ignitionCoord = ignitionIn simIn
        let cell' = cell { csStatus = LIVING }
        e <- edge -< ignitionCoord == return coord
        returnA -< (cell', e `tag` cell')

cellBurningSwitch :: CellState -> SF SimulationIn CellState
cellBurningSwitch cell = switch (cellBurning cell) cellDead

cellBurning :: CellState -> SF SimulationIn (CellState, Event CellState)
cellBurning cell = proc simIn ->
    do
        fuel <- integral >>^ (+ csFuel cell) -< -0.01
        e <- edge -< fuel <= 0.0
        let cell' = cell { csFuel = fuel, csStatus = BURNING }
        returnA -< (cell', e `tag` cell')

cellDead :: CellState -> SF SimulationIn CellState
cellDead cell = (constant cell { csStatus = DEAD })


createCells :: (Int, Int) -> [CellState]
createCells (xDim, yDim) = [ CellState {
    csCoord = (x,y),
    csFuel = cellFuelFunc (x,y) (xDim, yDim),
    csStatus = LIVING }
    | y <- [0..yDim - 1], x <- [0..xDim - 1] ]

cellFuelFunc :: (Int, Int) -> (Int, Int) -> Double
cellFuelFunc = cellFuelFuncMax

cellFuelFuncMax :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncMax coords dimensions = 1.0

cellFuelFuncSphere :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncSphere (x, y) (xDim, yDim) = fromIntegral (x^2 + y^2) / maxFunc
    where
        maxFunc = fromIntegral (xDim^2 + yDim^2)

idxOfCoord :: CellCoord -> (Int, Int) -> Int
idxOfCoord (xIdx, yIdx) (xDim, yDim) = (yIdx * xDim) + xIdx
