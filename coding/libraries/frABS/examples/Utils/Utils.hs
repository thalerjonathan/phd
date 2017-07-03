module Utils.Utils (
    ifThenElse,
    ifThenElseM,

    writeSirsDynamicsFile,
    sirsDynamicToString,
    sirsDynamicsReplMean
  ) where

import Text.Printf
import System.IO

ifThenElse :: Monad m => Bool -> m a -> m a -> m a
ifThenElse p trueAction falseAction = if p then trueAction else falseAction

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM test trueAction falseAction = test >>= \t -> if t then trueAction else falseAction

writeSirsDynamicsFile :: String 
                            -> Int
                            -> Double
                            -> Int
                            -> [(Double, Double, Double)] -> IO ()
writeSirsDynamicsFile fileName steps samplingTimeDelta replications dynamics =
    do
        fileHdl <- openFile fileName WriteMode
        hPutStrLn fileHdl ("steps = " ++ show steps ++ ";")
        hPutStrLn fileHdl ("dt = " ++ show samplingTimeDelta ++ ";")
        hPutStrLn fileHdl ("replications = " ++ show replications ++ ";")
        
        hPutStrLn fileHdl "dynamics = ["
        mapM_ (hPutStrLn fileHdl . sirsDynamicToString) dynamics
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "susceptibleRatio = dynamics (:, 1);"
        hPutStrLn fileHdl "infectedRatio = dynamics (:, 2);"
        hPutStrLn fileHdl "recoveredRatio = dynamics (:, 3);"
        hPutStrLn fileHdl "figure"
        hPutStrLn fileHdl "plot (susceptibleRatio.', 'color', 'green');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (infectedRatio.', 'color', 'red');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (recoveredRatio.', 'color', 'blue');"
        hPutStrLn fileHdl "xlabel ('Steps');"
        hPutStrLn fileHdl "ylabel ('Ratio');"
        hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"
        hPutStrLn fileHdl ("title ('SIRS Dynamics with " ++ show samplingTimeDelta ++ " dt, " ++ show steps ++ " steps, " ++  (show replications) ++ " replications');")

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