module Main where

import Control.Monad.Random
import System.IO
import Text.Printf

type Time         = Double
type TimeDelta    = Double

data SIRState 
  = Susceptible 
  | Infected TimeDelta
  | Recovered deriving (Show, Eq)

type SIRAgent     = SIRState
type Agents       = [SIRAgent]

agentCount :: Int
agentCount = 1000

infectedCount :: Int
infectedCount = 5

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

rngSeed :: Int
rngSeed = 42

t :: Time
t = 150.0

main :: IO ()
main = do
  setStdGen $ mkStdGen rngSeed

  as <- evalRandIO $ initAgentsRand agentCount infectedCount
  ass <- evalRandIO $ runSimulationUntil t as
  -- (tEnd, ass) <- evalRandIO $ runSimulation as
  -- putStrLn $ "All Recovered after t = " ++ show tEnd
  
  let dyns = aggregateAllStates ass
  let fileName =  "STEP_1_RANDMNONAD_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g 
                    => Time 
                    -> Agents 
                    -> Rand g [Agents]
runSimulationUntil tEnd as = runSimulationUntilAux 0 as []
  where
    runSimulationUntilAux :: RandomGen g 
                          => Time 
                          -> Agents 
                          -> [Agents] 
                          -> Rand g [Agents]
    runSimulationUntilAux t as acc
      | t >= tEnd = return $ reverse (as : acc)
      | otherwise = do
        as' <- stepSimulation as 
        runSimulationUntilAux (t + 1.0) as' (as : acc)

runSimulation :: RandomGen g 
              => Agents 
              -> Rand g (Time, [Agents])
runSimulation as = runSimulationAux 0 as []
  where
    runSimulationAux :: RandomGen g 
                      => Time 
                      -> Agents 
                      -> [Agents] 
                      -> Rand g (Time, [Agents])
    runSimulationAux t as acc
      | noneInfected as = return (t, reverse $ as : acc)
      | otherwise = do
          as' <- stepSimulation as 
          runSimulationAux (t + 1.0) as' (as : acc)

    noneInfected :: Agents -> Bool
    noneInfected as = not $ any isInfected as

stepSimulation :: RandomGen g => Agents -> Rand g Agents
stepSimulation as = mapM (processAgent as) as

processAgent :: RandomGen g 
              => Agents 
              -> SIRAgent 
              -> Rand g SIRAgent
processAgent as Susceptible    = susceptibleAgent as
processAgent _  (Infected dur) = return $ infectedAgent dur 
processAgent _  Recovered      = return Recovered

-- NOTE: does not exclude contact with itself but with a sufficiently large number 
-- of agents the probability becomes very small
-- NOTE: it is important that every contact with an other infected agent
-- can infect the agent and not just the first contact
susceptibleAgent :: RandomGen g => Agents -> Rand g SIRAgent
susceptibleAgent as = do
    rc <- randomExpM (1 / contactRate)
    cs <- replicateM (floor rc) (makeContact as)
    if or cs
      then infect
      else return Susceptible

  where
    makeContact :: RandomGen g => Agents -> Rand g Bool
    makeContact as = do
      randContact <- randomElem as
      case randContact of
        (Infected _) -> randomBoolM infectivity
        _            -> return False
      
    infect :: RandomGen g => Rand g SIRAgent
    infect = randomExpM (1 / illnessDuration) >>= \randIllDur -> return (Infected randIllDur)

infectedAgent :: TimeDelta -> SIRAgent
infectedAgent dur
    | dur' <= 0 = Recovered
    | otherwise = Infected dur'
  where
    dur' = dur - 1.0  

doTimes :: Monad m => Int -> m a -> m [a]
doTimes n f = forM [0..n - 1] (const f)

isSusceptible :: SIRAgent -> Bool
isSusceptible Susceptible = True
isSusceptible _           = False

isInfected :: SIRAgent -> Bool
isInfected (Infected _) = True
isInfected _            = False

isRecovered :: SIRAgent -> Bool
isRecovered Recovered = True
isRecovered _         = False

initAgentsRand :: RandomGen g => Int -> Int -> Rand g Agents
initAgentsRand n i = do
  let sus = replicate (n - i) Susceptible
  expTs <- replicateM i (randomExpM (1 / illnessDuration))
  let inf = map Infected expTs
  return $ sus ++ inf

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return ((-log r) / lambda))
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

aggregateAllStates :: [[SIRState]] -> [(Double, Double, Double)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter isSusceptible as
    infectedCount = fromIntegral $ length $ filter isInfected as
    recoveredCount = fromIntegral $ length $ filter isRecovered as

writeAggregatesToFile :: String -> [(Double, Double, Double)] -> IO ()
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

sirAggregateToString :: (Double, Double, Double) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"