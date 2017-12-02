module AfterExp 
    (
        testAfterExp
      , testAfterExpSuperSampling
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

    writeAfterExpFile avgDts eventTime

testAfterExpSuperSampling :: IO ()
testAfterExpSuperSampling = do
    let eventTime =  1 / 5      :: DTime
    let reps = 10000
    let dt = 1.0

    let ns = [ 1, 2, 5, 10, 100, 1000 ] :: [Int]

    let avgs = map (superSampleAfterExp eventTime reps dt) ns
    let avgTs = zip ns avgs

    writeAfterExpSSFile avgTs eventTime

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

superSampleAfterExp :: DTime -> Int -> DTime -> Int -> Double
superSampleAfterExp eventTime reps dt n = sum ts / fromIntegral (length ts)
  where
    ts = parMap rpar (runAfterExpSuperSampled n eventTime dt) [1..reps]

    runAfterExpSuperSampled :: Int -> DTime -> DTime -> Int -> DTime
    runAfterExpSuperSampled n expEventTime dt seed = actualEventime
      where
        g = mkStdGen seed
        sf = afterExp g expEventTime () -- RandomGen g => g -> DTime -> b -> SF a (Event b)
        ssSf = superSampling n sf

        deltas = repeat (dt, Nothing)
        bss = embed ssSf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

        bs = concat bss

        firstEventIdx = fromJust $ findIndex isEvent bs
        firstEventIdxSSAdjusted = fromIntegral firstEventIdx / fromIntegral n
        actualEventime = dt * firstEventIdxSSAdjusted

writeAfterExpFile :: [(Double, Double)]
                      -> DTime
                      -> IO ()
writeAfterExpFile avgDts eventTime = do
    let fileName = "samplingTest_afterExp_" ++ show eventTime ++ "time.m"

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

writeAfterExpSSFile :: [(Int, Double)]
                        -> DTime
                        -> IO ()
writeAfterExpSSFile  avgDts eventTime = do
    let fileName = "samplingTest_afterExp_ss_" ++ show eventTime ++ "time.m"

    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dtAvgs = ["
    mapM_ (hPutStrLn fileHdl . avgDtToString) avgDts
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl ("eventTime = " ++ show eventTime ++ ";")
    hPutStrLn fileHdl "ss = dtAvgs (:, 1);"
    hPutStrLn fileHdl "avg = dtAvgs (:, 2);"

    hPutStrLn fileHdl "n = length (ss);"
    hPutStrLn fileHdl "ecsTheoryLinePoints = n + 1;"
    hPutStrLn fileHdl "ecsTheoryLineX = [0; ss];"
    hPutStrLn fileHdl "ecsTheoryLineY = ones(ecsTheoryLinePoints, 1) * eventTime;"

    hPutStrLn fileHdl "figure;"
    hPutStrLn fileHdl "semilogx (ss, avg, 'color', 'blue', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (ecsTheoryLineX, ecsTheoryLineY, 'color', 'red', 'linewidth', 2);"

    hPutStrLn fileHdl "xLabels = cellstr(num2str(ss));"
    hPutStrLn fileHdl "yLabels = cellstr(num2str(avg));"

    hPutStrLn fileHdl "set(gca,'YTick', avg);"
    hPutStrLn fileHdl "set(gca,'XTick', ss);"
    hPutStrLn fileHdl "set(gca, 'xticklabel', xLabels);"
    hPutStrLn fileHdl "set(gca, 'yticklabel', yLabels);"

    hPutStrLn fileHdl "xlabel ('Super Samples');"
    hPutStrLn fileHdl "ylabel ('Average Time-Out');"
    hPutStrLn fileHdl "legend ('Average Time-Out per Super Samples', 'Theoretical Maximum');"
    hPutStrLn fileHdl ("title ('Super-Sampling afterExp with Time-Delta of 1.0 and average timeout of " ++ show eventTime ++ " time-units');")

    hClose fileHdl

  where
    avgDtToString :: (Int, Double) -> String
    avgDtToString (dt, avg) = 
        printf "%d" dt 
            ++ "," ++ printf "%.3f" avg
            ++ ";" 