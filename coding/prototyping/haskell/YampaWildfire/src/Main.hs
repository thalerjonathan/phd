{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Data.List
import System.Environment (getArgs, getProgName)

import WildFireFrontend as Front
import WildFireBackend as Back

dimensions :: (Int, Int)
dimensions = (10, 10)

-- NOTE: reactimate version
main :: IO ()
main = do
    Front.init
    let cells = (Back.initCells dimensions)
    reactimate (return cells) (input cells) output Back.process
    Front.shutdown

-- TODO: ignite a cell after a given time
input :: [Cell] -> Bool -> IO (DTime, Maybe [Cell])
input cells _ = do
    coords <- Front.checkMousePress
    case coords of
        Just c -> do
            let cellIdx = Front.pixelCoordToCellIdx dimensions c
            let cell = cells !! cellIdx
            let cell' = cell { cellState = BURNING, cellFuel = 1.0 }
            let (frontList, backList) = splitAt cellIdx cells
            let newCells = (Data.List.init frontList) ++ [cell'] ++ backList
            return (1.0, Just newCells)
        Nothing -> do
            return (1.0, Nothing)

output :: Bool -> [Cell] -> IO Bool
output _ newCells = do
    Front.renderFrame newCells dimensions
    return False

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