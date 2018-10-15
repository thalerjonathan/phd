module SIR 
  ( SIRSimCtx (..)
  , SIRState (..)

  , defaultSIRCtx
  , sirAggregate

  , initAgents

  , writeAggregatesToFile
  ) where

import System.IO
import Text.Printf
import Control.Monad.Random

data SIRSimCtx g = SIRSimCtx
  { syCtxTimeLimit   :: !Double
  , syCtxTimeDelta   :: !Double

  , syCtxRng         :: g

  , syCtxSusceptible :: !Int
  , syCtxInfected    :: !Int
  , syCtxRecovered   :: !Int

  , syCtxContactRate :: !Double
  , syCtxInfectivity :: !Double
  , syCtxIllnessDur  :: !Double
  }

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

defaultSIRCtx :: RandomGen g 
              => g
              -> SIRSimCtx g 
defaultSIRCtx g = SIRSimCtx {
    syCtxTimeLimit   = 200
  , syCtxTimeDelta   = 0.01

  , syCtxRng         = g

  , syCtxSusceptible = 999
  , syCtxInfected    = 1
  , syCtxRecovered   = 0

  , syCtxContactRate = 5
  , syCtxInfectivity = 0.05
  , syCtxIllnessDur  = 15
  }

sirAggregate :: [SIRState] -> (Double, Double, Double)
sirAggregate as = (sus, inf, recs)
  where
    sus  = fromIntegral $ length $ filter (==Susceptible) as
    inf  = fromIntegral $ length $ filter (==Infected) as
    recs = fromIntegral $ length $ filter (==Recovered) as

initAgents :: Int -> Int -> Int -> [SIRState]
initAgents s i r = sus ++ inf ++ recs
  where
    sus = replicate s Susceptible
    inf = replicate i Infected
    recs = replicate r Recovered

writeAggregatesToFile :: String 
                      -> Double
                      -> [(Double, Double, Double)] 
                      -> IO ()
writeAggregatesToFile fileName dt dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirAggregateToString) dynamics
  hPutStrLn fileHdl "];"

  writeMatlabPlot fileHdl dt

  hClose fileHdl

writeMatlabPlot :: Handle 
                -> Double
                -> IO ()
writeMatlabPlot fileHdl dt = do
  hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  hPutStrLn fileHdl "infected = dynamics (:, 2);"
  hPutStrLn fileHdl "recovered = dynamics (:, 3);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "steps = length (susceptible);"
  hPutStrLn fileHdl "indices = 0 : steps - 1;"
  hPutStrLn fileHdl $ "indices = indices ./ " ++ show (1 / dt) ++ ";"

  hPutStrLn fileHdl "figure"
  hPutStrLn fileHdl "plot (indices, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, infectedRatio.', 'color', 'red', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

  hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
  
  hPutStrLn fileHdl "xlabel ('Time');"
  hPutStrLn fileHdl "ylabel ('Population Ratio');"
  hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

sirAggregateToString :: (Double, Double, Double) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"