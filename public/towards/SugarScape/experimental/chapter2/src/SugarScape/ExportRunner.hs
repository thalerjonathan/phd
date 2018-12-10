module SugarScape.ExportRunner
  ( writeSimulationUntil
  ) where

import System.IO
import System.Random

import FRP.BearRiver

import SugarScape.Model 
import SugarScape.Simulation

writeSimulationUntil :: RandomGen g
                     => String
                     -> Time
                     -> SimulationState g
                     -> IO ()
writeSimulationUntil fileName tMax ss0 = do
    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dynamics = {"
    writeSimulationUntilAux ss0 fileHdl
    hPutStrLn fileHdl "};"
    hClose fileHdl
  where
    writeSimulationUntilAux :: RandomGen g
                            => SimulationState g
                            -> Handle
                            -> IO ()
    writeSimulationUntilAux ss fileHdl 
        | t >= tMax = return ()
        | otherwise = do
          hPutStrLn fileHdl ("{ " ++ show t ++ ",")
          mapM_ writeAgentObservable aobs
          hPutStrLn fileHdl "}"
          writeSimulationUntilAux ss' fileHdl
      where
        (ss', (t, _, aobs)) = simulationStep ss

        writeAgentObservable :: AgentObservable SugAgentObservable -> IO ()
        writeAgentObservable (aid, ao) 
            = hPutStrLn fileHdl ("[" ++ show aid ++ ", " ++ 
                                 show age ++ ", " ++
                                 show sug ++ ", " ++
                                 show met ++ ", " ++ 
                                 show vis ++ "], ")
          where
            vis = sugObsVision ao
            age = sugObsAge ao
            sug = sugObsSugLvl ao
            met = sugObsSugMetab ao