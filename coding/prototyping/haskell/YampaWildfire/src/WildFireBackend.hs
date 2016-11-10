{-# LANGUAGE Arrows #-}

module WildFireBackend where

import FRP.Yampa
import FRP.Yampa.Switches

dimensions :: (Int, Int)
dimensions = (10, 10)

center :: (Int, Int)
center = (centerX, centerY)
    where
        (dimX, dimY) = dimensions
        centerX = floor( fromIntegral dimX / 2.0 )
        centerY = floor( fromIntegral dimY / 2.0 )

neighbourhood :: [CellCoord]
neighbourhood = [top] -- [topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight]
    where
        topLeft = (-1, -1)
        top = (0, -1)
        topRight = (1, -1)
        left = (-1, 0)
        right = (1, 0)
        bottomLeft = (-1, 1)
        bottom = (0, 1)
        bottomRight = (1, 1)

data CellStatus = LIVING | BURNING | DEAD deriving (Eq)
type CellCoord = (Int, Int)

data CellState = CellState {
    csFuel :: Double,
    csStatus :: CellStatus,
    csCoord :: CellCoord
}

data CellInput = CellInput {
}

data CellOutput = CellOutput {
    coState :: CellState,
    coDead :: Event (),
    coIgniteNeighbours :: [CellCoord]
}

instance Eq CellState where
    c1 == c2 = csCoord c1 == csCoord c2

type BurningCell = SF CellInput CellOutput

data SimulationIn = SimulationIn {
    simInIgnitions :: [CellCoord]
}

data SimulationOut = SimulationOut {
    simOutCellStates :: [CellState]
}

-- NOTE: problem with performance: when having 1000x1000 grid then its extremely slow. Speed-up: need only running SF
-- in case of BURNING, otherwise behaviour is constant. SF will start on ignition through neighbour and stop when
-- all fuel has burned
process :: [CellState] -> SF SimulationIn SimulationOut
process allCellStates = proc simIn ->
    do
        rec
            cellOutputs <- (procHelper []) -< (simIn, outputStates)
            let outputStates = map coState cellOutputs
        returnA -< SimulationOut{ simOutCellStates = outputStates }

procHelper :: [CellState] -> SF (SimulationIn, [CellState]) [CellOutput]
procHelper burningCellStates = dpSwitch
                                    route                                   -- Routing function
                                    (cellStatesToCell burningCellStates)    -- collection of signal functions.
                                    (arr burnOrDie >>> notYet)              -- Signal function that observes the external input signal and the output signals from the collection in order to produce a switching event.
                                    continuation                            -- Continuation to be invoked once event occurs.


{- Routing function. Its purpose is to pair up each running signal function
in the collection maintained by dpSwitch with the input it is going to see
at each point in time. All the routing function can do is specify how the input is distributed.
-}
route :: (SimulationIn, [CellState]) -> [sf] -> [(CellInput, sf)]
route (simIn, allCellStates) cellSFs = map (\sf -> (CellInput, sf)) cellSFs

-- creates the initial collection of signal functions.
cellStatesToCell :: [CellState] -> [BurningCell]
cellStatesToCell cellStates = map burningCell cellStates

{- Signal function that observes the external input signal and
the output signals from the collection in order to produce a switching event.
-}
burnOrDie :: ((SimulationIn, [CellState]), [CellOutput]) -> (Event [CellState])
burnOrDie ((simIn, allCellStates), cellOuts)
    | (null inputIgnitedCells) && (null ignitedCells) && (null diedCells) = noEvent
    | otherwise = Event (removeDuplicates (inputIgnitedCells ++ ignitedCells)) -- important: if dead-event then an event must be emited even if list is empty then
    where
        inputIgnitedCells = filter (markedForIgnition simIn) $ filter isLiving allCellStates
        ignitedCells = filter (ignitedByNeighbour cellOuts) $ filter isLiving allCellStates
        diedCells = filter died cellOuts

{- The fourth argument is a function that is invoked when the switching event occurs,
yielding a new signal function to switch into based on the collection of signal functions
previously running and the value carried by the switching event. This allows the collection
 to be updated and then switched back in, typically by employing dpSwitch again.
-}
continuation :: [BurningCell] -> [CellState] -> SF (SimulationIn, [CellState]) [CellOutput]
continuation cellSFs burningCellStates = procHelper burningCellStates


burningCell :: CellState -> BurningCell
burningCell cs = proc ci ->
    do
        fuel <- integral >>^ (+ csFuel cs) -< -0.01
        dead <- edge -< fuel <= 0.0
        returnA -< CellOutput {
            coDead = dead,
            coState = cs { csStatus = BURNING, csFuel = fuel },
            coIgniteNeighbours = [] } --neighbours (csCoord cs) }

neighbours :: CellCoord -> [CellCoord]
neighbours cc = filter (not . clip) $ map (addCoord cc) neighbourhood

addCoord :: CellCoord -> CellCoord -> CellCoord
addCoord (cX, cY) (cX', cY') = (cX + cX', cY + cY')

clip :: CellCoord -> Bool
clip (cx, cy)
    | cx < 0 || cy < 0 = True
    | cx >= (fst dimensions) = True
    | cy >= (snd dimensions) = True
    | otherwise = False

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates as = foldl (\acc a -> if elem a acc then acc else acc ++ [a] ) [] as

died :: CellOutput -> Bool
died co = coDead co /= NoEvent

isLiving :: CellState -> Bool
isLiving cs = csStatus cs == LIVING

ignitedByNeighbour :: [CellOutput] -> CellState -> Bool
ignitedByNeighbour cellOuts cs = any (\co -> ignite (coIgniteNeighbours co) cs ) cellOuts

ignite :: [CellCoord] -> CellState -> Bool
ignite ignitionCoords cs = any (\c -> c == cellCoord) ignitionCoords
    where
        cellCoord = csCoord cs

markedForIgnition :: SimulationIn -> CellState -> Bool
markedForIgnition simIn cs = ignite ignitions cs
    where
        ignitions = simInIgnitions simIn

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
