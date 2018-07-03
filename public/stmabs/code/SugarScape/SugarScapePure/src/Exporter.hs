module Exporter 
  (
    writeSugarscapeDynamics
  ) where

import FRP.BearRiver

import System.IO
import Text.Printf

import Model

writeSugarscapeDynamics :: [(Time, [SugAgentState], SugEnvironment)] -> IO ()
writeSugarscapeDynamics dynamics = do
  writeWealthDistribution dynamics
  writeCarryingCapacity dynamics
  writeCulturalDynamics dynamics

writeWealthDistribution :: [(Time, [SugAgentState], SugEnvironment)] -> IO ()
writeWealthDistribution dynamics = do
  let steps = length dynamics 
  let (_, finalAos, _) = last dynamics

  fileHdl <- openFile "sugarscape_wealthDist.m" WriteMode
  
  hPutStrLn fileHdl "wealthDist = ["
  mapM_ (hPutStrLn fileHdl . (\ao -> show $ sugAgSugarLevel ao)) finalAos
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "figure;"
  hPutStrLn fileHdl "hist(wealthDist);"
  hPutStrLn fileHdl "xlabel ('Wealth');"
  hPutStrLn fileHdl "ylabel ('Agents');"
  hPutStrLn fileHdl ("title ('Wealth-Distribution after " ++ show steps ++ " steps');")

  hClose fileHdl

writeCarryingCapacity :: [(Time, [SugAgentState], SugEnvironment)] -> IO ()
writeCarryingCapacity dynamics = do
  let steps = length dynamics 

  fileHdl <- openFile "sugarscape_carryingcapacity.m" WriteMode
  
  hPutStrLn fileHdl "carryingcapacity = ["
  mapM_ (hPutStrLn fileHdl . (\(t, aos, _) -> "" ++ show t ++ "," ++ show (length aos))) dynamics
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "figure;"
  hPutStrLn fileHdl "plot (carryingcapacity.', 'color', 'blue');"
  hPutStrLn fileHdl "xlabel ('Steps');"
  hPutStrLn fileHdl "ylabel ('Agents');"
  hPutStrLn fileHdl $ "title ('Carrying Capacity over " ++ show steps ++ " steps');"

  hClose fileHdl

writeCulturalDynamics :: [(Time, [SugAgentState], SugEnvironment)] -> IO ()
writeCulturalDynamics dynamics = do
    fileHdl <- openFile "sugarscape_culturaldynamics.m" WriteMode
    
    hPutStrLn fileHdl "dynamics = ["
    mapM_ (hPutStrLn fileHdl . (tupleToString . tribeFractions . (\(_, aos, _) -> aos))) dynamics
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl "redFract = dynamics (:, 1);"
    hPutStrLn fileHdl "blueFract = dynamics (:, 2);"
    hPutStrLn fileHdl "figure"
    hPutStrLn fileHdl "plot (redFract.', 'color', 'red');"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (blueFract.', 'color', 'blue');"
    hPutStrLn fileHdl "xlabel ('Steps');"
    hPutStrLn fileHdl "ylabel ('Fraction');"
    hPutStrLn fileHdl "legend('Red Tribe','Blue Tribe');"
    hPutStrLn fileHdl "title ('Cultural Dynamics');"

    hClose fileHdl
  where
    tribeFractions :: [SugAgentState] -> (Double, Double)
    tribeFractions aobs = (redTribeFract, blueTribeFract)
      where
        redTribesCount = length $ filter (\ao -> Red == sugAgTribe ao) aobs
        blueTribesCount = length $ filter (\ao -> Blue == sugAgTribe ao) aobs

        redTribeFract = fromIntegral redTribesCount / fromIntegral (redTribesCount + blueTribesCount)
        blueTribeFract = fromIntegral blueTribesCount / fromIntegral (redTribesCount + blueTribesCount)

tupleToString :: (Double, Double) -> String
tupleToString (x, y)
  = printf "%.3f" x 
    ++ "," ++ printf "%.3f" y
    ++ ";"