module Main where

import FRP.Yampa
import System.Environment (getArgs, getProgName)

import WildFireFrontend as Front
import WildFireBackend as Back

dimensions :: (Int, Int)
dimensions = (10, 10)

center :: (Int, Int)
center = (centerX, centerY)
    where
        (dimX, dimY) = dimensions
        centerX = floor( fromIntegral dimX / 2.0 )
        centerY = floor( fromIntegral dimY / 2.0 )

-- NOTE: reactimate version
main :: IO ()
main = do
    Front.initialize
    let cell = Cell { cellCoord = center, cellFuel = 0.5, cellState = LIVING }
    reactimate (return cell) input output (Back.process cell)
    Front.shutdown

input :: Bool -> IO (DTime, Maybe Cell)
input _ = return (0.2, Nothing)

output :: Bool -> Cell -> IO Bool
output _ newCell = do
    Front.renderFrame [newCell] dimensions
    return False

{-
initialize :: IO IgniteCellAction
initialize = return IgniteCellAction{ cellIdx = 50, cells = Back.createCells dimensions }

input :: Bool -> IO (DTime, Maybe IgniteCellAction)
input _ = return (1.0, Nothing)
-}

{-
input :: [Cell] -> Bool -> IO (DTime, Maybe [Cell])
input cells _ = do
    coords <- Front.checkMousePress
    case coords of
        Just c -> do
            cellIdx <- Front.pixelCoordToCellIdx dimensions c
            let cell = cells !! cellIdx
            let cell' = cell { cellState = BURNING }
            let (frontList, backList) = splitAt cellIdx cells
            let newCells = (init frontList) ++ [cell'] ++ backList
            return (1.0, Just newCells)
        Nothing -> do
            return (1.0, Nothing)
-}

-- NOTE: reactInit/ract version
{-
main :: IO ()
main = do
    Front.init
    hdl <- reactInit initialize output Back.process
    inputLoop hdl
    Front.shutdown
    return ()

initialize :: IO [Cell]
initialize = Back.initCells (10, 10)

output :: ReactHandle [Cell] [Cell] -> Bool -> [Cell] -> IO Bool
output hdl changed newCells = do
                    Front.renderFrame newCells
                    return True

inputLoop :: ReactHandle [Cell] [Cell] -> IO ()
inputLoop hdl = do
    ret <- react hdl (1.0, Nothing)
    if ret == True
        then
            return ()
        else
            inputLoop hdl
-}