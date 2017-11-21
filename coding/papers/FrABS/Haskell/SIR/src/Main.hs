module Main where

import Data.List
import Data.Maybe
import Control.Monad.Random
import System.Random
import System.IO
import Text.Printf

-- an agent is in one of these states at any time
data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
-- an agent has a state and a time this state is valid for
type SIRAgent = (SIRState, Time)
type Agents = [SIRAgent]

type Time = Double
type TimeDelta = Double

contactRate :: Double
contactRate = 5.0

infectionProb :: Double
infectionProb = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentCount :: Int
agentCount = 5000

infectedCount :: Int
infectedCount = 10

rngSeed :: Int
rngSeed = 42

dt :: TimeDelta
dt = 1.0

t :: Time
t = 150.0

susceptible :: SIRAgent
susceptible = (Susceptible, 0)

infected :: Time -> SIRAgent
infected t = (Infected, t)

recovered :: SIRAgent
recovered = (Recovered, 0)

main :: IO ()
main = do
  setStdGen $ mkStdGen rngSeed

  as <- evalRandIO $ initAgents agentCount infectedCount
  -- as' <- evalRandIO $ runSimulationUntil t dt as
  (tEnd, ass) <- evalRandIO $ runSimulation dt as
  
  putStrLn $ "All Recovered after t = " ++ show tEnd
  
  let dyns = aggregateDynamics ass
  let fileName =  "SIR_SIMPLE_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeDynamicsToFile fileName dyns

runSimulationUntil :: (RandomGen g) => Time -> TimeDelta -> Agents -> Rand g [Agents]
runSimulationUntil tEnd dt as = runSimulationUntilAux tEnd 0 dt as []
  where
    runSimulationUntilAux :: (RandomGen g) => Time -> Time -> TimeDelta -> Agents -> [Agents] -> Rand g [Agents]
    runSimulationUntilAux tEnd t dt as acc
      | t >= tEnd = return $ reverse (as : acc)
      | otherwise = do
        as' <- stepSimulation dt as 
        runSimulationUntilAux tEnd (t + dt) dt as' (as : acc)

runSimulation :: (RandomGen g) => TimeDelta -> Agents -> Rand g (Time, [Agents])
runSimulation dt as = runSimulationAux 0 dt as []
  where
    runSimulationAux :: (RandomGen g) => Time -> TimeDelta -> Agents -> [Agents] -> Rand g (Time, [Agents])
    runSimulationAux t dt as acc
      | noInfected as = return $ (t, reverse $ as : acc)
      | otherwise = do
        as' <- stepSimulation dt as 
        runSimulationAux (t + dt) dt as' (as : acc)

    noInfected :: Agents -> Bool
    noInfected as = not $ any (is Infected) as

stepSimulation :: (RandomGen g) => TimeDelta -> Agents -> Rand g Agents
stepSimulation dt as = mapM (processAgent dt as) as

processAgent :: (RandomGen g) => TimeDelta -> Agents -> SIRAgent -> Rand g SIRAgent
processAgent _ as (Susceptible, _) = susceptibleAgent as
processAgent dt _ a@(Infected, _) = return $ infectedAgent dt a
processAgent _ _ a@(Recovered, _) = return a

-- NOTE: does not exclude contact with itself but with a sufficiently large number of agents the probability becomes very small
susceptibleAgent :: (RandomGen g) => Agents -> Rand g SIRAgent
susceptibleAgent as = do
    randContactCount <- randomExpM (1 / contactRate)
    a <- doTimes (floor randContactCount) (susceptibleAgentAux as)
    let mayInf = find (is Infected) a
    return $ fromMaybe susceptible mayInf

  where
    susceptibleAgentAux :: (RandomGen g) => Agents -> Rand g SIRAgent
    susceptibleAgentAux as = do
      randContact <- drawRandomAgent as
      if (is Infected randContact)
        then infect
        else return susceptible

    infect :: (RandomGen g) => Rand g SIRAgent
    infect = do
      doInfect <- randomBoolM infectionProb
      randIllDur <- randomExpM (1 / illnessDuration)
      if doInfect
        then return $ infected randIllDur
        else return susceptible

infectedAgent :: TimeDelta -> SIRAgent -> SIRAgent
infectedAgent dt (_, t) 
    | t' <= 0 = recovered
    | otherwise = infected t'
  where
    t' = t - dt    

doTimes :: (Monad m) => Int -> m a -> m [a]
doTimes n f = forM [0..n - 1] (\_ -> f) 

drawRandomAgent :: (RandomGen g) => Agents -> Rand g SIRAgent
drawRandomAgent as = getRandomR (0, len - 1) >>= (\idx -> return $ as !! idx)
  where
    len = length as

is :: SIRState -> SIRAgent -> Bool
is s (s',_) = s == s'

initAgents :: (RandomGen g) => Int -> Int -> Rand g Agents
initAgents n i = do
  let sus = replicate (n - i) susceptible
  expTs <- mapM randomExpM (replicate i (1 / illnessDuration))
  let inf = map infected expTs
  return $ sus ++ inf  

randomBoolM :: (RandomGen g) => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: (RandomGen g) => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r

aggregateDynamics :: [Agents] -> [(Double, Double, Double)]
aggregateDynamics ass = map aggregate ass

aggregate :: Agents -> (Double, Double, Double)
aggregate as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter ((Susceptible==) . fst) as
    infectedCount = fromIntegral $ length $ filter ((Infected==) . fst) as
    recoveredCount = fromIntegral $ length $ filter ((Recovered==) . fst) as

writeDynamicsToFile :: String -> [(Double, Double, Double)] -> IO ()
writeDynamicsToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirDynamicToString) dynamics
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

sirDynamicToString :: (Double, Double, Double) -> String
sirDynamicToString (susceptibleRatio, infectedRatio, recoveredRatio) = 
  printf "%.3f" susceptibleRatio 
  ++ "," ++ printf "%.3f" infectedRatio
  ++ "," ++ printf "%.3f" recoveredRatio
  ++ ";" 