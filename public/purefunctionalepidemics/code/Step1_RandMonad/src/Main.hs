module Main where

import Control.Monad.Random

import SIR

type Time         = Double
type TimeDelta    = Double

type SIRAgent     = (SIRState, Time)
type Agents       = [SIRAgent]

agentCount :: Int
agentCount = 10000

infectedCount :: Int
infectedCount = 5

rngSeed :: Int
rngSeed = 42

dt :: TimeDelta
dt = 1.0

t :: Time
t = 150.0

main :: IO ()
main = do
  setStdGen $ mkStdGen rngSeed

  as <- evalRandIO $ initAgentsRand agentCount infectedCount
  ass <- evalRandIO $ runSimulationUntil t dt as
  -- (tEnd, ass) <- evalRandIO $ runSimulation dt as
  -- putStrLn $ "All Recovered after t = " ++ show tEnd
  
  let dyns = aggregateAllStates (map (map fst) ass)
  let fileName =  "STEP_1_RANDMNONAD_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g 
                    => Time 
                    -> TimeDelta 
                    -> Agents 
                    -> Rand g [Agents]
runSimulationUntil tEnd dt as = runSimulationUntilAux 0 as []
  where
    runSimulationUntilAux :: RandomGen g 
                          => Time 
                          -> Agents 
                          -> [Agents] 
                          -> Rand g [Agents]
    runSimulationUntilAux t as acc
      | t >= tEnd = return $ reverse (as : acc)
      | otherwise = do
        as' <- stepSimulation dt as 
        runSimulationUntilAux (t + dt) as' (as : acc)

runSimulation :: RandomGen g 
              => TimeDelta 
              -> Agents 
              -> Rand g (Time, [Agents])
runSimulation dt as = runSimulationAux 0 dt as []
  where
    runSimulationAux :: RandomGen g 
                      => Time 
                      -> TimeDelta 
                      -> Agents 
                      -> [Agents] 
                      -> Rand g (Time, [Agents])
    runSimulationAux t dt as acc
      | noneInfected as = return (t, reverse $ as : acc)
      | otherwise = do
          as' <- stepSimulation dt as 
          runSimulationAux (t + dt) dt as' (as : acc)

    noneInfected :: Agents -> Bool
    noneInfected as = not $ any (is Infected) as

stepSimulation :: RandomGen g => TimeDelta -> Agents -> Rand g Agents
stepSimulation dt as = mapM (processAgent dt as) as

processAgent :: RandomGen g 
              => TimeDelta 
              -> Agents 
              -> SIRAgent 
              -> Rand g SIRAgent
processAgent _  as    (Susceptible, _) = susceptibleAgent as
processAgent dt _   a@(Infected   , _) = return $ infectedAgent dt a
processAgent _  _   a@(Recovered  , _) = return a

-- NOTE: does not exclude contact with itself but with a sufficiently large number 
-- of agents the probability becomes very small
-- NOTE: it is important that every contact with an other infected agent
-- can infect the agent and not just the first contact
susceptibleAgent :: RandomGen g => Agents -> Rand g SIRAgent
susceptibleAgent as = do
    rc <- randomExpM (1 / contactRate)
    -- cs <- doTimes (floor rc) (makeContact as)
    cs <- forM ([0..floor rc - 1] :: [Int]) (const (makeContact as))
    if True `elem`cs
      then infect
      else return susceptible

  where
    makeContact :: RandomGen g => Agents -> Rand g Bool
    makeContact as = do
      randContact <- randomElem as
      case fst randContact of
        Infected -> randomBoolM infectivity
        _        -> return False
      
    infect :: RandomGen g => Rand g SIRAgent
    infect = randomExpM (1 / illnessDuration) >>= \randIllDur -> return (Infected, randIllDur)

infectedAgent :: TimeDelta -> SIRAgent -> SIRAgent
infectedAgent dt (_, t) 
    | t' <= 0   = recovered
    | otherwise = infected t'
  where
    t' = t - dt  

doTimes :: Monad m => Int -> m a -> m [a]
doTimes n f = forM [0..n - 1] (const f)

is :: SIRState -> SIRAgent -> Bool
is s (s',_) = s == s'

initAgentsRand :: RandomGen g => Int -> Int -> Rand g Agents
initAgentsRand n i = do
  let sus = replicate (n - i) susceptible
  expTs <- replicateM i (randomExpM (1 / illnessDuration))
  let inf = map infected expTs
  return $ sus ++ inf

susceptible :: SIRAgent
susceptible = (Susceptible, 0)

infected :: Time -> SIRAgent
infected t = (Infected, t)

recovered :: SIRAgent
recovered = (Recovered, 0)

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