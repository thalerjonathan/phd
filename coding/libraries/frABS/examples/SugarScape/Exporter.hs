module SugarScape.Exporter (
    writeSugarscapeDynamics
  ) where

import FRP.FrABS

import SugarScape.Model

import System.IO
import Text.Printf

writeSugarscapeDynamics :: [([SugarScapeAgentOut], SugarScapeEnvironment)] -> IO ()
writeSugarscapeDynamics dynamics =
    do
        writeWealthDistribution dynamics
        writeCarryingCapacity dynamics
        writeCulturalDynamics dynamics

writeWealthDistribution :: [([SugarScapeAgentOut], SugarScapeEnvironment)] -> IO ()
writeWealthDistribution dynamics =
    do
        let steps = length dynamics 
        let (finalAos, _) = last dynamics

        fileHdl <- openFile "sugarscape_wealthDist.m" WriteMode
        
        hPutStrLn fileHdl "wealthDist = ["
        mapM_ (hPutStrLn fileHdl . (\ao -> show $ sugAgSugarLevel $ aoState ao)) finalAos
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "figure;"
        hPutStrLn fileHdl "hist(wealthDist);"
        hPutStrLn fileHdl "xlabel ('Wealth');"
        hPutStrLn fileHdl "ylabel ('Agents');"
        hPutStrLn fileHdl ("title ('Wealth-Distribution after " ++ show steps ++ " steps');")

        hClose fileHdl

writeCarryingCapacity :: [([SugarScapeAgentOut], SugarScapeEnvironment)] -> IO ()
writeCarryingCapacity dynamics =
    do
        let steps = length dynamics 

        fileHdl <- openFile "sugarscape_carryingcapacity.m" WriteMode
        
        hPutStrLn fileHdl "carryingcapacity = ["
        mapM_ (hPutStrLn fileHdl . (\(aos, _) -> show $ length aos)) dynamics
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "figure;"
        hPutStrLn fileHdl "plot (carryingcapacity.', 'color', 'blue');"
        hPutStrLn fileHdl "xlabel ('Steps');"
        hPutStrLn fileHdl "ylabel ('Agents');"
        hPutStrLn fileHdl ("title ('Carrying Capacity over " ++ show steps ++ " steps');")

        hClose fileHdl

writeCulturalDynamics :: [([SugarScapeAgentOut], SugarScapeEnvironment)] -> IO ()
writeCulturalDynamics dynamics =
    do
        let steps = length dynamics 

        fileHdl <- openFile "sugarscape_culturaldynamics.m" WriteMode
        
        hPutStrLn fileHdl "dynamics = ["
        mapM_ (hPutStrLn fileHdl . (tupleToString . tribeFractions . fst)) dynamics
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
        hPutStrLn fileHdl ("title ('Cultural Dynamics');")


        hClose fileHdl

    where
        tribeFractions :: [SugarScapeAgentOut] -> (Double, Double)
        tribeFractions aos = (redTribeFract, blueTribeFract)
            where
                redTribesCount = length $ filter (\ao -> Red == (sugAgTribe $ aoState ao)) aos
                blueTribesCount = length $ filter (\ao -> Blue == (sugAgTribe $ aoState ao)) aos

                redTribeFract = fromIntegral redTribesCount / fromIntegral (redTribesCount + blueTribesCount)
                blueTribeFract = fromIntegral blueTribesCount / fromIntegral (redTribesCount + blueTribesCount)

tupleToString :: (Double, Double) -> String
tupleToString (x, y) = 
                printf "%.3f" x 
                    ++ "," ++ printf "%.3f" y
                    ++ ";"