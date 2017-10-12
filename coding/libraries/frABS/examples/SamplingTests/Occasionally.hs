module Occasionally 
    (
        testOccasionally
      , testOccasionallySuperSampling
    ) where 

import Text.Printf
import System.Random
import System.IO
import Control.Parallel.Strategies

import FRP.Yampa

import SuperSampling

testOccasionally :: IO () 
testOccasionally = do
    let t = 1000            :: DTime
    let eventFreq =  5   :: DTime
    let eventsPerTimeUnit = 1 / eventFreq 
    let ecsTheory = t * eventsPerTimeUnit
    let reps = 100

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

    let avgs = map (sampleOccasionally t eventFreq reps) dts
    let avgDts = zip dts avgs

    writeOccasionallyFile avgDts eventFreq ecsTheory

testOccasionallySuperSampling :: IO () 
testOccasionallySuperSampling = do
    let t = 100            :: DTime
    let eventFreq =  1 / 5   :: DTime
    let eventsPerTimeUnit = 1 / eventFreq 
    let ecsTheory = t * eventsPerTimeUnit
    let reps = 100
    let dt = 1.0

    let ns = [ 1, 2, 5, 10, 100, 1000 ] 

    let avgs = map (superSampleOccasionally t eventFreq reps dt) ns
    let avgSs = zip ns avgs

    writeOccasionallySSFile avgSs eventFreq ecsTheory

sampleOccasionally :: DTime -> DTime -> Int -> DTime -> Double
sampleOccasionally t eventFreq reps dt = fromIntegral (sum ecs) / fromIntegral reps :: Double
  where
    steps = floor (t / dt) :: Int
    ecs = parMap rpar (runOccasionally eventFreq dt steps) [1..reps]

    runOccasionally :: DTime -> DTime -> Int ->  Int -> Int
    runOccasionally eventFreq dt steps seed = eventCount
      where
        g = mkStdGen seed
        sf = occasionally g eventFreq () -- RandomGen g => g -> Time -> b -> SF a (Event b)

        deltas = replicate steps (dt, Nothing)
        bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

        eventCount = (length . filter isEvent) bs

superSampleOccasionally :: DTime -> DTime -> Int -> DTime -> Int -> Double
superSampleOccasionally t eventFreq reps dt n = fromIntegral (sum ecs) / fromIntegral (reps ) :: Double
  where
    steps = floor (t / dt) :: Int
    ecs = parMap rpar (runOccasionallySS eventFreq dt steps n) [1..reps]

    runOccasionallySS :: DTime -> DTime -> Int -> Int -> Int -> Int
    runOccasionallySS eventFreq dt steps n seed = eventCount
      where
        g = mkStdGen seed
        sf = occasionally g eventFreq () -- RandomGen g => g -> Time -> b -> SF a (Event b)
        ssSf = superSampling n sf

        deltas = replicate steps (dt, Nothing)
        bss = embed ssSf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

        bs = concat bss
        eventCount = (length . filter isEvent) bs

writeOccasionallyFile :: [(DTime, Double)]
                        -> DTime
                        -> DTime
                        -> IO ()
writeOccasionallyFile avgDts eventFreq ecsTheory = do
    let fileName = "samplingTest_occasionally_" ++ show (1 / eventFreq) ++ "evts.m"

    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dtAvgs = ["
    mapM_ (hPutStrLn fileHdl . avgDtToString) avgDts
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl ("ecsTheory = " ++ show ecsTheory ++ ";")
    hPutStrLn fileHdl "dt = dtAvgs (:, 1);"
    hPutStrLn fileHdl "avg = dtAvgs (:, 2);"
    hPutStrLn fileHdl "minDt = min(dt);"
    hPutStrLn fileHdl "maxDt = max(dt);"

    hPutStrLn fileHdl "n = length (dt);"
    hPutStrLn fileHdl "ecsTheoryLinePoints = n + 1;"
    hPutStrLn fileHdl "ecsTheoryLineX = [0; dt];"
    hPutStrLn fileHdl "ecsTheoryLineY = ones(ecsTheoryLinePoints, 1) * ecsTheory;"

    hPutStrLn fileHdl "figure;"
    hPutStrLn fileHdl "semilogx (dt, avg, 'color', 'blue', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (ecsTheoryLineX, ecsTheoryLineY, 'color', 'red', 'linewidth', 2);"

    hPutStrLn fileHdl "yTickUpperLimit = ecsTheory * 1.1;"
    hPutStrLn fileHdl "axis ([0 maxDt 0 yTickUpperLimit]);"
    hPutStrLn fileHdl "axis ('auto x');"

    hPutStrLn fileHdl "xLabels = cellstr(num2str(dt));"

    hPutStrLn fileHdl "set(gca,'YTick', 0:500:yTickUpperLimit);"
    hPutStrLn fileHdl "set(gca,'YTick', avg);"
    hPutStrLn fileHdl "set(gca,'XTick', dt);"
    hPutStrLn fileHdl "set(gca, 'xticklabel', xLabels);"

    hPutStrLn fileHdl "xlabel ('Time-Deltas');"
    hPutStrLn fileHdl "ylabel ('Average Events');"
    hPutStrLn fileHdl "legend ('Average Events per Time-Deltas', 'Theoretical Maximum');"
    hPutStrLn fileHdl ("title ('Sampling occasionally with " ++ show (1 / eventFreq) ++ " events per time-unit');")

    hClose fileHdl

  where
    avgDtToString :: (DTime, Double) -> String
    avgDtToString (dt, avg) = 
        printf "%.3f" dt 
            ++ "," ++ printf "%.3f" avg
            ++ ";" 

writeOccasionallySSFile :: [(Int, Double)]
                        -> DTime
                        -> DTime
                        -> IO ()
writeOccasionallySSFile avgDts eventFreq ecsTheory = do
    let fileName = "samplingTest_occasionally_ss_" ++ show (1 / eventFreq) ++ "evts.m"

    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dtAvgs = ["
    mapM_ (hPutStrLn fileHdl . avgDtToString) avgDts
    hPutStrLn fileHdl "];"

    hPutStrLn fileHdl ("ecsTheory = " ++ show ecsTheory ++ ";")
    hPutStrLn fileHdl "dt = dtAvgs (:, 1);"
    hPutStrLn fileHdl "avg = dtAvgs (:, 2);"
    hPutStrLn fileHdl "minDt = min(dt);"
    hPutStrLn fileHdl "maxDt = max(dt);"

    hPutStrLn fileHdl "n = length (dt);"
    hPutStrLn fileHdl "ecsTheoryLinePoints = n + 1;"
    hPutStrLn fileHdl "ecsTheoryLineX = [0; dt];"
    hPutStrLn fileHdl "ecsTheoryLineY = ones(ecsTheoryLinePoints, 1) * ecsTheory;"

    hPutStrLn fileHdl "figure;"
    hPutStrLn fileHdl "semilogx (dt, avg, 'color', 'blue', 'linewidth', 2);"
    hPutStrLn fileHdl "hold on"
    hPutStrLn fileHdl "plot (ecsTheoryLineX, ecsTheoryLineY, 'color', 'red', 'linewidth', 2);"

    hPutStrLn fileHdl "yTickUpperLimit = ecsTheory * 1.1;"
    hPutStrLn fileHdl "axis ([0 maxDt 0 yTickUpperLimit]);"
    hPutStrLn fileHdl "axis ('auto x');"

    hPutStrLn fileHdl "xLabels = cellstr(num2str(dt));"

    hPutStrLn fileHdl "set(gca,'YTick', 0:500:yTickUpperLimit);"
    hPutStrLn fileHdl "set(gca,'YTick', avg);"
    hPutStrLn fileHdl "set(gca,'XTick', dt);"
    hPutStrLn fileHdl "set(gca, 'xticklabel', xLabels);"

    hPutStrLn fileHdl "xlabel ('Super Samples');"
    hPutStrLn fileHdl "ylabel ('Average Events');"
    hPutStrLn fileHdl "legend ('Average Events per Super Samples', 'Theoretical maximum');"
    hPutStrLn fileHdl ("title ('Super-Sampling occasionally using time-delta of 1.0 with " ++ show (1 / eventFreq) ++ " events per time-unit');")

    hClose fileHdl

  where
    avgDtToString :: (Int, Double) -> String
    avgDtToString (ss, avg) = 
        printf "%d" ss 
            ++ "," ++ printf "%.3f" avg
            ++ ";" 