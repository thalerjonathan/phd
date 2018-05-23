module Main where

import Control.Monad
import System.Random
import System.IO
import Text.Printf

import ABSFeedback
import SIRRandMonad
import SD
import SIR

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

rngSeed :: Int
rngSeed = 42

maxInfLastRec :: [(Double, Double, Double)] 
              -> ((Integer, Double), Integer)
maxInfLastRec dyns = maxInfLastRecAux dyns 0 (-1, 0) (-1)
  where
    maxInfLastRecAux :: [(Double, Double, Double)] 
                     -> Integer
                     -> (Integer, Double)
                     -> Integer
                     -> ((Integer, Double), Integer)
    maxInfLastRecAux [] _ maxInf lastRecIdx = (maxInf, lastRecIdx) 
    maxInfLastRecAux ((_, i, _) : dyns) idx maxInf@(_, maxInfValue) lastRecIdx 
      = let maxInf' = if i > maxInfValue then (idx, i) else maxInf
        in if i < 1 
              then (maxInf', idx) 
              else maxInfLastRecAux dyns (idx + 1) maxInf' lastRecIdx

main :: IO ()
main = do
  setStdGen $ mkStdGen rngSeed

  let popSize      = 100 :: Double
      infCount     = 1 :: Double
      replications = 10000 :: Int
      t            = 150 :: Double
      absDt        = 1.0 :: Double
      sdDt         = 0.1 :: Double

  let sdDyns  = runSD popSize infCount contactRate infectivity illnessDuration t sdDt
  let ((maxInfIdx, maxInfValue), lastRecIdx) = maxInfLastRec sdDyns

  let maxInfTime = fromIntegral maxInfIdx * sdDt
  let lastRecTime = fromIntegral lastRecIdx * sdDt

  putStrLn $ "SD maxInf: " ++ show (maxInfTime, maxInfValue)
  putStrLn $ "SD lastRec: " ++ show lastRecTime

  (maxInf, lastRec) <- foldM (\(maxInfAcc, lastRecAcc) _i -> do
      g' <- newStdGen
      --let absDyns = runFeedbackABS g' (floor popSize) (floor infCount) contactRate infectivity illnessDuration t absDt
      let absDyns = runSirRandMonad g' (floor popSize) (floor infCount) contactRate infectivity illnessDuration t
      
      let ((maxInfIdx, maxInfValue), lastRecIdx) = maxInfLastRec absDyns

      let maxInfTime = fromIntegral maxInfIdx * absDt
      let lastRecTime = fromIntegral lastRecIdx * absDt

      return ((maxInfTime, maxInfValue) : maxInfAcc, lastRecTime : lastRecAcc)
    ) (([],[]) :: ([(Double, Double)], [Double])) [1..replications]

  let absfilename = "sir_randmonad_" ++ show popSize ++ "pop_" ++ show replications ++ "repls_" ++ show absDt ++ "dt.m"
  writeMatlabFile absfilename (maxInfTime, maxInfValue, lastRecTime) maxInf lastRec

writeMatlabFile :: String 
                -> (Double, Double, Double)
                -> [(Double, Double)] 
                -> [Double]-> IO ()
writeMatlabFile fileName (sdMaxInfTime, sdMaxInfValue, sdLastRecTime) maxInf lastRec = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "maxInf = ["
  mapM_ (hPutStrLn fileHdl . timeValueToString) maxInf
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "lastRec = ["
  mapM_ (hPutStrLn fileHdl . (\t -> printf "%f" t ++ ";")) lastRec
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl ("sdMaxInfTime = " ++ printf "%f" sdMaxInfTime ++ ";")
  hPutStrLn fileHdl ("sdMaxInfValue = " ++ printf "%f" sdMaxInfValue ++ ";")
  hPutStrLn fileHdl ("sdLastRecTime = " ++ printf "%f" sdLastRecTime ++ ";")

  hPutStrLn fileHdl "maxInfTimes = maxInf(:, 1);"
  hPutStrLn fileHdl "maxInfValues = maxInf(:, 2);"

  hPutStrLn fileHdl "lastRecTimes = lastRec(:, 1);"

  hPutStrLn fileHdl "maxInfTimesMean = mean(maxInfTimes);"
  hPutStrLn fileHdl "maxInfTimesMedian = median(maxInfTimes);"

  hPutStrLn fileHdl "maxInfValuesMean = mean(maxInfValues);"
  hPutStrLn fileHdl "maxInfValuesMedian = median(maxInfValues);"

  hPutStrLn fileHdl "lastRecTimesMean = mean(lastRecTimes);"
  hPutStrLn fileHdl "lastRecTimesMedian = median(lastRecTimes);"

  hPutStrLn fileHdl "figure"
  hPutStrLn fileHdl "subplot (2, 2, 1);"
  hPutStrLn fileHdl "hist(maxInfValues, 20);"
  hPutStrLn fileHdl "title (sprintf ('Max Infected Time\\n mean = %d, median = %d', maxInfTimesMean, maxInfTimesMedian));"

  hPutStrLn fileHdl "subplot (2, 2, 2);"
  hPutStrLn fileHdl "hist(maxInfTimes, 20);"
  hPutStrLn fileHdl "title (sprintf ('Max Infected Value\\n mean = %d, median = %d', maxInfValuesMean, maxInfValuesMedian));"

  hPutStrLn fileHdl "subplot (2, 2, 3);"
  hPutStrLn fileHdl "hist(lastRecTimes, 20);"
  hPutStrLn fileHdl "title (sprintf ('Last Recovery Time\\n mean = %d, median = %d', lastRecTimesMean, lastRecTimesMedian));"

  hClose fileHdl

timeValueToString :: (Double, Double) -> String
timeValueToString (t, v) =
  printf "%f" t
  ++ "," ++ printf "%f" v
  ++ ";"