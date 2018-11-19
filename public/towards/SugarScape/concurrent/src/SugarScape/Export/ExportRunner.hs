module SugarScape.Export.ExportRunner
  ( writeSimulationUntil
  ) where

import System.IO
import System.Random
import Text.Printf

import SugarScape.Core.Common
import SugarScape.Core.Model
import SugarScape.Core.Simulation

progressBarLength :: Int
progressBarLength = 40

writeSimulationUntil :: RandomGen g
                     => String
                     -> Time
                     -> SimulationState g
                     -> IO ()
writeSimulationUntil fileName tMax ss0 = do
    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "clear;\ndynamics = {"
    writeSimulationUntilAux ss0 fileHdl
    hPutStrLn fileHdl "};"
    hClose fileHdl
  where
    writeSimulationUntilAux :: RandomGen g
                            => SimulationState g
                            -> Handle
                            -> IO ()
    writeSimulationUntilAux ss fileHdl = do
        (ss', (t, _, aobs)) <- simulationTick ss
        
        printProgress t

        hPutStrLn fileHdl ("{ " ++ show t ++ ",")
        mapM_ writeAgentObservable aobs
        hPutStrLn fileHdl "}"

        hFlush fileHdl

        if t >= tMax
          then return ()
          else writeSimulationUntilAux ss' fileHdl

      where
        printProgress :: Time -> IO () 
        printProgress t = do
          let progRatio  = (fromIntegral t / fromIntegral tMax) :: Double
              percentage = 100 * progRatio
            
              barElems   = floor (progRatio * fromIntegral progressBarLength)
              barSpace   = progressBarLength - barElems
              progBar    = "|" ++ replicate barElems '=' ++ replicate barSpace ' ' ++ "|"
          
          putStr $ "\r" ++ progBar ++ printf " %.1f" percentage ++ "%"
          

        writeAgentObservable :: AgentObservable SugAgentObservable -> IO ()
        writeAgentObservable (aid, ao) 
            = hPutStrLn fileHdl ("[" ++ show aid ++ ", " ++ 
                                 show age ++ ", " ++
                                 show sug ++ ", " ++
                                 show spi ++ ", " ++ 
                                 show met ++ ", " ++ 
                                 show vis ++ ", " ++
                                 show gen ++ ", {" ++
                                 show cult ++ "}, " ++
                                 trades ++ "], ")
          where
            vis = sugObsVision ao
            age = sugObsAge ao
            sug = sugObsSugLvl ao
            spi = sugObsSpiLvl ao
            met = sugObsSugMetab ao
            gen = case sugObsGender ao of
                    Male   -> 0 :: Int
                    Female -> 1 :: Int
            cult = map (\tag -> if tag then 1 else 0) (sugObsCultureTag ao) :: [Int]
            trades = "{[" ++ concatMap tradeInfoToString (sugObsTrades ao) ++ "]}"
    
    tradeInfoToString :: TradeInfo -> String
    tradeInfoToString (TradeInfo price sugar spice _) = show price ++ ", " ++ show sugar ++ ", " ++ show spice ++ ","
