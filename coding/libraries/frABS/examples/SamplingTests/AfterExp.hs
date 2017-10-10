module AfterExp 
    (
      testAfterExp
    ) where

import Data.Maybe
import Data.List
import Text.Printf
import System.Random
import System.IO
import Control.Parallel.Strategies

import FRP.FrABS
import FRP.Yampa

testAfterExp :: IO ()
testAfterExp = do
    let eventTime =  1 / 5      :: DTime
    let reps = 10000

    let dts = [ 
                 5
               , 2 
               , 1
               , 1 / 2
               , 1 / 5
               , 1 / 10
               , 1 / 20
               , 1 / 50
               , 1 / 100
               ] 

    let avgs = map (sampleAfterExp eventTime reps) dts
    let avgDts = zip dts avgs

    let fileName = "samplingTest_afterExp_" ++ show eventTime ++ "time.m"
    writeAfterExpFile fileName avgDts eventTime

sampleAfterExp :: DTime -> Int -> DTime -> Double
sampleAfterExp eventTime reps dt = sum ts / fromIntegral (length ts)
  where
    ts = parMap rpar (runAfterExp eventTime dt) [1..reps]

    runAfterExp :: DTime -> DTime -> Int -> DTime
    runAfterExp expEventTime dt seed = actualEventime
      where
        g = mkStdGen seed
        sf = afterExp g expEventTime () -- RandomGen g => g -> DTime -> b -> SF a (Event b)

        deltas = repeat (dt, Nothing)
        bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

        firstEventIdx = fromIntegral $ fromJust $ findIndex isEvent bs
        actualEventime = dt * firstEventIdx

writeAfterExpFile :: String 
                        -> [(DTime, Double)]
                        -> DTime
                        -> IO ()
writeAfterExpFile fileName avgDts eventTime = do
    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dtAvgs = ["
    mapM_ (hPutStrLn fileHdl . avgDtToString) avgDts
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl ("eventTime = " ++ show eventTime ++ ";")
    hPutStrLn fileHdl "dt = dtAvgs (:, 1);"
    hPutStrLn fileHdl "avg = dtAvgs (:, 2);"

    hPutStrLn fileHdl "n = length (dt);"
    hPutStrLn fileHdl "ecsTheoryLinePoints = n + 1;"
    hPutStrLn fileHdl "ecsTheoryLineX = [0; dt];"
    hPutStrLn fileHdl "ecsTheoryLineY = ones(ecsTheoryLinePoints, 1) * eventTime;"

    hPutStrLn fileHdl "figure;"
    hPutStrLn fileHdl "semilogx (dt, avg, 'color', 'blue', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (ecsTheoryLineX, ecsTheoryLineY, 'color', 'red', 'linewidth', 2);"

    hPutStrLn fileHdl "xLabels = cellstr(num2str(dt));"
    hPutStrLn fileHdl "yLabels = cellstr(num2str(avg));"

    hPutStrLn fileHdl "set(gca,'YTick', avg);"
    hPutStrLn fileHdl "set(gca,'XTick', dt);"
    hPutStrLn fileHdl "set(gca, 'xticklabel', xLabels);"
    hPutStrLn fileHdl "set(gca, 'yticklabel', yLabels);"

    hPutStrLn fileHdl "xlabel ('Time-Deltas');"
    hPutStrLn fileHdl "ylabel ('Average Time-Out');"
    hPutStrLn fileHdl "legend ('Average Time-Out per Time-Deltas', 'Theoretical Maximum');"
    hPutStrLn fileHdl ("title ('Sampling afterExp with average timeout of " ++ show eventTime ++ " time-units');")

    hClose fileHdl

  where
    avgDtToString :: (DTime, Double) -> String
    avgDtToString (dt, avg) = 
        printf "%.3f" dt 
            ++ "," ++ printf "%.3f" avg
            ++ ";" 