module Utils.Sir (
    writeSirDynamicsFile
  , sirDynamicToString
  , sirDynamicsReplMean
  ) where

import Text.Printf
import System.IO

import FRP.Yampa

writeSirDynamicsFile :: String 
                        -> Double
                        -> Int
                        -> [(Time, Double, Double, Double)] -> IO ()
writeSirDynamicsFile fileName dt replications dynamics = do
    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dynamics = ["
    mapM_ (hPutStrLn fileHdl . sirDynamicToString) dynamics
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl "time = dynamics (:, 1);"
    hPutStrLn fileHdl "susceptible = dynamics (:, 2);"
    hPutStrLn fileHdl "infected = dynamics (:, 3);"
    hPutStrLn fileHdl "recovered = dynamics (:, 4);"
    hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

    hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
    hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
    hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

    hPutStrLn fileHdl "steps = length (time);"
    hPutStrLn fileHdl "indices = 0 : steps - 1;"

    hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")

    hPutStrLn fileHdl "figure"
    hPutStrLn fileHdl "plot (indices, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (indices, infectedRatio.', 'color', 'red', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (indices, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

    hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
    hPutStrLn fileHdl "set(gca,'XTick', indices(1:10:end), 'xticklabel', time(1:10:end));"

    hPutStrLn fileHdl "xlabel ('Time');"
    hPutStrLn fileHdl "ylabel ('Population Ratio');"
    hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

    hPutStrLn fileHdl ("titleStr = sprintf('SIR Dynamics of %i agents, with "
        ++ printf "%.2f" dt ++ " dt, %i steps, " 
        ++ show replications ++ " replications', totalPopulation, steps)");

    hPutStrLn fileHdl "title (titleStr);"

    hClose fileHdl

sirDynamicToString :: (Time, Double, Double, Double) -> String
sirDynamicToString (t, susceptibleRatio, infectedRatio, recoveredRatio) = 
    printf "%.2f" t 
        ++ "," ++ printf "%.3f" susceptibleRatio 
        ++ "," ++ printf "%.3f" infectedRatio
        ++ "," ++ printf "%.3f" recoveredRatio
        ++ ";" 

sirDynamicsReplMean :: [[(Time, Double, Double, Double)]] -> [(Time, Double, Double, Double)]
sirDynamicsReplMean [] = []
sirDynamicsReplMean replDynamics@(initRepl:tailRepls) = replDynamicsRatio
  where
    replCountRational = fromIntegral $ length replDynamics:: Double

    replDynamicsSum = foldr sumDyns initRepl tailRepls
    replDynamicsRatio = map (\(t, s, i, r) -> (t, s / replCountRational, i / replCountRational, r / replCountRational)) replDynamicsSum

    sumDyns :: [(Time, Double, Double, Double)] -> [(Time, Double, Double, Double)] -> [(Time, Double, Double, Double)]
    sumDyns ds1 ds2 = map (\((t, s1, i1, r1), (_, s2, i2, r2)) -> (t, s1+s2, i1+i2, r1+r2)) (zip ds1 ds2)