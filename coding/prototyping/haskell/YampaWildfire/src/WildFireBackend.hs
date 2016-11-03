{-# LANGUAGE Arrows #-}

module WildFireBackend where

import FRP.Yampa

data CellState = LIVING | BURNING | DEAD deriving (Eq)
type CellCoord = (Int, Int)

data Cell = Cell
    {
        cellFuel :: Double,
        cellState :: CellState,
        cellCoord :: CellCoord
    }

data IgniteCellAction = IgniteCellAction
    {
        cellIdx :: Int,
        cells :: [Cell]
    }

process :: Cell -> SF Cell Cell
process cell = switch (cellLiving cell) cellBurning

cellLiving :: Cell -> SF Cell (Cell, Event Cell)
cellLiving cell = proc c ->
    do
        t <- time -< 0.0
        e <- edge -< t >= 10.0
        let cell' = cell { cellState = LIVING }
        returnA -< (cell', e `tag` cell')

cellBurning :: Cell -> SF Cell Cell
cellBurning cell = switch (cellBurning' cell) cellDead

cellBurning' :: Cell -> SF Cell (Cell, Event Cell)
cellBurning' cell = proc c ->
    do
        fuel <- integral >>^ (+ cellFuel cell) -< -0.01
        e <- edge -< fuel <= 0.0
        let cell' = cell { cellFuel = fuel, cellState = BURNING }
        returnA -< (cell', e `tag` cell')

cellDead :: Cell -> SF Cell Cell
cellDead cell = (constant cell { cellState = DEAD })

createCells :: (Int, Int) -> [Cell]
createCells (xDim, yDim) = [ Cell { cellCoord = (x,y), cellFuel = cellFuelFunc (x,y) (xDim, yDim), cellState = LIVING } | y <- [0..yDim - 1], x <- [0..xDim - 1] ]

cellFuelFunc :: (Int, Int) -> (Int, Int) -> Double
cellFuelFunc = cellFuelFuncSphere

cellFuelFuncMax :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncMax coords dimensions = 1.0

cellFuelFuncSphere :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncSphere (x, y) (xDim, yDim) = fromIntegral (x^2 + y^2) / maxFunc
    where
        maxFunc = fromIntegral (xDim^2 + yDim^2)

