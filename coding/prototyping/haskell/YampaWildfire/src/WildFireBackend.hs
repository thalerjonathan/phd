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

process :: SF [Cell] [Cell]
process = proc cs -> do
                cs' <- identity -< cs
                returnA -< cs'

initCells :: (Int, Int) -> [Cell]
initCells (xDim, yDim) = [ Cell { cellCoord = (x,y), cellFuel = cellFuelFunc (x,y) (xDim, yDim), cellState = LIVING } | x <- [0..xDim - 1], y <- [0..yDim - 1] ]

cellFuelFunc :: (Int, Int) -> (Int, Int) -> Double
cellFuelFunc = cellFuelFuncMax

cellFuelFuncMax :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncMax coords dimensions = 1.0

cellFuelFuncSphere :: (Int, Int) -> (Int, Int) -> Double
cellFuelFuncSphere (x, y) (xDim, yDim) = fromIntegral (x^2 + y^2) / maxFunc
    where
        maxFunc = fromIntegral (xDim^2 + yDim^2)


{-
1. call to initialize
2. return of initialize is fed to process
3. return of process is fed to output
4. input is called which produces the next sample / time or returns Nothing when no change occured
5. the return-value of input is fed to process
6. jump to 3

main :: IO ()
main = reactimate initialize input output process

-- initializes the system: returns data which is then sent to process
initialize :: IO Double
initialize  = return 0.0

-- receives a Bool which is unused
-- return Tuple with DTime: (time elapsed since last input, Maybe input-data)
input :: Bool -> IO (DTime, Maybe Double)
input _ = return (1.0, Just 0.2)

-- 1st argument: a Bool which is unused
-- 2nd argument: the output from process
-- return True when terminating programm
output :: Bool -> Double -> IO Bool
output _ x = putStrLn (show x) >> return (x >= 10.0)

-- the process-function: maps input-data to output-data
process :: SF Double Double
process = proc input -> do
                t0' <- time -< input
                returnA -< t0'
-}

