module Main where

import System.IO
import Text.Printf

import SIR
import PureRunner
--import VisualRunner

main :: IO ()
main = do
  let populationSize  = 1000
      infectedCount   = 1
      contactRate     = 5
      infectivity     = 0.05
      illnessDuration = 15

      --freq            = 10000
      t               = 100
      dt              = 0.0001

  let ret      = runSDPure populationSize infectedCount contactRate infectivity illnessDuration t dt
  --runSDVisual freq populationSize infectedCount contactRate infectivity illnessDuration t dt
      fileName = "sir_sd_" ++ show populationSize ++ "_" ++ show dt ++ "dt.m"
  writeSIRStepsToFile fileName ret

writeSIRStepsToFile :: String -> [SIRStep] -> IO ()
writeSIRStepsToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirStepToString) dynamics
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "time = dynamics (:, 1);"
  hPutStrLn fileHdl "susceptible = dynamics (:, 2);"
  hPutStrLn fileHdl "infected = dynamics (:, 3);"
  hPutStrLn fileHdl "recovered = dynamics (:, 4);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "figure"
  hPutStrLn fileHdl "plot (time, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (time, infectedRatio.', 'color', 'red', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (time, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

  hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
  
  hPutStrLn fileHdl "xlabel ('Time');"
  hPutStrLn fileHdl "ylabel ('Population Ratio');"
  hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

  hClose fileHdl

sirStepToString :: SIRStep -> String
sirStepToString (t, susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" t
  ++ "," ++ printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"