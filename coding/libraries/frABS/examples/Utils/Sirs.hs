module Utils.Sirs 
    (
      writeSirsDynamicsFile
    , sirsDynamicToString
    , sirsDynamicsReplMean
    ) where

import Text.Printf
import System.IO

writeSirsDynamicsFile :: String 
                        -> Int
                        -> Double
                        -> Int
                        -> [(Double, Double, Double)] -> IO ()
writeSirsDynamicsFile fileName steps samplingTimeDelta replications dynamics = do
    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dynamics = ["
    mapM_ (hPutStrLn fileHdl . sirsDynamicToString) dynamics
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl ("steps = " ++ show steps ++ ";")
    hPutStrLn fileHdl ("dt = " ++ show samplingTimeDelta ++ ";")
    hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")

    hPutStrLn fileHdl "startingTime = 0;"
    hPutStrLn fileHdl "endTime = steps * dt;"
    hPutStrLn fileHdl "time = 0 : dt : endTime;"

    hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
    hPutStrLn fileHdl "infected = dynamics (:, 2);"
    hPutStrLn fileHdl "recovered = dynamics (:, 3);"
    hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

    hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
    hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
    hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

    hPutStrLn fileHdl "figure"
    hPutStrLn fileHdl "plot (time, susceptibleRatio.', 'color', 'blue', 'linewidth', 3);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (time, infectedRatio.', 'color', 'red', 'linewidth', 3);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (time, recoveredRatio.', 'color', 'green', 'linewidth', 3);"

    hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0)"
    hPutStrLn fileHdl "set(gca,'XTick',0:10:endTime)"

    hPutStrLn fileHdl "xlabel ('Time');"
    hPutStrLn fileHdl "ylabel ('Population Ratio');"
    hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

    hPutStrLn fileHdl ("titleStr = sprintf('SIR Dynamics of %i agents, with "
        ++ printf "%.2f" samplingTimeDelta ++ " dt, " 
        ++ show steps ++ " steps, " 
        ++ show replications ++ " replications', totalPopulation)");

    hPutStrLn fileHdl "title (titleStr);"

    hClose fileHdl

sirsDynamicToString :: (Double, Double, Double) -> String
sirsDynamicToString dynamics = 
    printf "%.3f" susceptibleRatio 
        ++ "," ++ printf "%.3f" infectedRatio
        ++ "," ++ printf "%.3f" recoveredRatio
        ++ ";" 
  where
    (susceptibleRatio, infectedRatio, recoveredRatio) = dynamics 

sirsDynamicsReplMean :: [[(Double, Double, Double)]] -> [(Double, Double, Double)]
sirsDynamicsReplMean [] = []
sirsDynamicsReplMean replDynamics@(initRepl:tailRepls) = replDynamicsRatio
  where
    replCountRational = fromIntegral $ length replDynamics :: Double

    replDynamicsSum = foldr sumDyns initRepl tailRepls
    replDynamicsRatio = map (\(s, i, r) -> (s / replCountRational, i / replCountRational, r / replCountRational)) replDynamicsSum

    sumDyns :: [(Double, Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
    sumDyns ds1 ds2 = map (\((s1, i1, r1), (s2, i2, r2)) -> (s1+s2, i1+i2, r1+r2)) (zip ds1 ds2)