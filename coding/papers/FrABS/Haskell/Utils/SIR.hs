module SIR
  (
    SIRState (..)

  , initAgents
  
  , aggregateAllStates
  , aggregateStates
  , writeAggregatesToFile

  , randomBoolM
  , randomExpM
  , randomElem
  ) where

import System.IO
import Text.Printf

import Control.Monad.Random

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

aggregateAllStates :: [[SIRState]] -> [(Int, Int, Int)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Int, Int, Int)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = length $ filter (Susceptible==) as
    infectedCount = length $ filter (Infected==) as
    recoveredCount = length $ filter (Recovered==) as

writeAggregatesToFile :: String -> [(Int, Int, Int)] -> IO ()
writeAggregatesToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirAggregateToString) dynamics
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  hPutStrLn fileHdl "infected = dynamics (:, 2);"
  hPutStrLn fileHdl "recovered = dynamics (:, 3);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "steps = length (susceptible);"
  hPutStrLn fileHdl "indices = 0 : steps - 1;"

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

  hClose fileHdl

sirAggregateToString :: (Int, Int, Int) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%d" susceptibleCount
  ++ "," ++ printf "%d" infectedCount
  ++ "," ++ printf "%d" recoveredCount
  ++ ";"
  
randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r

randomElem :: RandomGen g => [a] -> Rand g a
randomElem as = getRandomR (0, len - 1) >>= (\idx -> return $ as !! idx)
  where
    len = length as