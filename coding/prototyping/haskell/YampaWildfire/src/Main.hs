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
    let cells = Back.createCells dimensions
    let process' = Back.process' cells
    reactimate Main.initialize (input ref) output process'
    Front.shutdown

dimensions :: (Int, Int)
dimensions = (100, 100)

center :: (Int, Int)
center = (centerX, centerY)
    where
        (dimX, dimY) = dimensions
        centerX = floor( fromIntegral dimX / 2.0 )
        centerY = floor( fromIntegral dimY / 2.0 )

initialize :: IO SimulationIn
initialize = do
    return SimulationIn { simInIgnitions = [] }

input :: IORef Bool -> Bool -> IO (DTime, Maybe SimulationIn)
input ref _ = do
    coords <- Front.checkMousePressed ref
    case coords of
      Just c -> do
          let cellCoord = Front.pixelCoordToCellCoord c dimensions
          return (1.0, Just SimulationIn { simInIgnitions = [cellCoord] } )
      Nothing -> do
          return (1.0, Just SimulationIn { simInIgnitions = [] } )

output :: Bool -> SimulationOut -> IO Bool
output _ out = do
    let cells = simOutCellStates out
    Front.renderFrame cells dimensions
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