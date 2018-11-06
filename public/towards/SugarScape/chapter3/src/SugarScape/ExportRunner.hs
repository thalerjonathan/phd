module SugarScape.ExportRunner
  ( writeSimulationUntil
  ) where

import System.IO
import System.Random

import SugarScape.Common
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
        (ss', (t, _, _, aobs)) = simulationStep ss

        writeAgentObservable :: AgentObservable SugAgentObservable -> IO ()
        writeAgentObservable (aid, ao) 
            = hPutStrLn fileHdl ("[" ++ show aid ++ ", " ++ 
                                 show age ++ ", " ++
                                 show sug ++ ", " ++
                                 show met ++ ", " ++ 
                                 show vis ++ ", " ++
                                 show gen ++ ", " ++
                                 show cult ++ "], ")
          where
            vis = sugObsVision ao
            age = sugObsAge ao
            sug = sugObsSugLvl ao
            met = sugObsSugMetab ao
            gen = case sugObsGender ao of
                    Male   -> 0 :: Int
                    Female -> 1 :: Int
            cult = map (\tag -> if tag then 1 else 0) (sugObsCultureTag ao) :: [Int]