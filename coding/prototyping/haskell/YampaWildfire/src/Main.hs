module Main where

import Data.IORef
import FRP.Yampa
import System.Environment (getArgs, getProgName)

import WildFireFrontend as Front
import WildFireBackend as Back

main :: IO ()
main = do
    Front.initialize
    ref <- newIORef True
    let cell = Cell { cellCoord = center, cellFuel = 1.0, cellState = LIVING } -- Back.createCells dimensions
    let input' = input ref
    let process' = (Back.process cell)
    reactimate Main.initialize input' output process'
    Front.shutdown

dimensions :: (Int, Int)
dimensions = (10, 10)

center :: (Int, Int)
center = (centerX, centerY)
    where
        (dimX, dimY) = dimensions
        centerX = floor( fromIntegral dimX / 2.0 )
        centerY = floor( fromIntegral dimY / 2.0 )

initialize :: IO SimulationIn
initialize = do
    return SimulationIn { ignitionIn = Nothing }

input :: IORef Bool -> Bool -> IO (DTime, Maybe SimulationIn)
input ref _ = do
    coords <- Front.checkMousePressed ref
    case coords of
      Just c -> do
          let cellCoord = Front.pixelCoordToCellCoord c dimensions
          return (1.0, Just SimulationIn { ignitionIn = Just cellCoord } )
      Nothing -> do
          return (1.0, Just SimulationIn { ignitionIn = Nothing } )

output :: Bool -> SimulationOut -> IO Bool
output _ out = do
    let cells = cellsOut out
    Front.renderFrame cells dimensions
    return False


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