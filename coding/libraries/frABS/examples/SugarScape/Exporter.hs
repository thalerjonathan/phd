module SugarScape.Exporter (
    writeSugarscapeDynamics
  ) where

import FRP.FrABS

import SugarScape.Model

import System.IO


-- TODO: export dynamics in a text file with matlab format of the data: wealth distribution, number of agents, mean vision/metabolism, mean age,


writeSugarscapeDynamics :: [([SugarScapeAgentOut], SugarScapeEnvironment)] -> IO ()
writeSugarscapeDynamics dynamics =
    do
        writeWealthDistribution dynamics
        writeCarryingCapacity dynamics

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

        