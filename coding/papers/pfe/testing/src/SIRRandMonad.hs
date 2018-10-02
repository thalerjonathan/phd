module SIRRandMonad 
  (
    runSirRandMonad
  ) where

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

runSirRandMonad :: RandomGen g 
                => g 
                -> Int
                -> Int
                -> Double
                -> Double
                -> Double 
                -> Double 
                -> [(Double, Double, Double)]
runSirRandMonad g populationSize infectedCount contactRate infectivity illnessDuration t 
    = aggregateAllStates $ evalRand (runSimulationUntil t dt as) g'
  where
    dt = 1.0 -- NOTE: this runs always with dt = 1.0
    (as, g') = runRand (initAgentsRand illnessDuration populationSize infectedCount) g

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

    stepSimulation :: RandomGen g => TimeDelta -> Agents -> Rand g Agents
    stepSimulation dt as = mapM (processAgent dt contactRate infectivity illnessDuration as) as

processAgent :: RandomGen g 
              => TimeDelta
              -> Double 
              -> Double 
              -> Double 
              -> Agents 
              -> SIRAgent 
              -> Rand g SIRAgent
processAgent _  contactRate infectivity illnessDuration as Susceptible = susceptibleAgent contactRate infectivity illnessDuration as
processAgent dt _ _ _ _  (Infected dur) = return $ infectedAgent dt dur 
processAgent _  _ _ _ _  Recovered      = return Recovered

-- NOTE: does not exclude contact with itself but with a sufficiently large number 
-- of agents the probability becomes very small
-- NOTE: it is important that every contact with an other infected agent
-- can infect the agent and not just the first contact
susceptibleAgent :: RandomGen g
                 => Double 
                 -> Double 
                 -> Double 
                 -> Agents 
                 -> Rand g SIRAgent
susceptibleAgent contactRate infectivity illnessDuration as = do
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

infectedAgent :: TimeDelta -> TimeDelta -> SIRAgent
infectedAgent dt dur
    | dur' <= 0 = Recovered
    | otherwise = Infected dur'
  where
    dur' = dur - dt  

isSusceptible :: SIRAgent -> Bool
isSusceptible Susceptible = True
isSusceptible _           = False

isInfected :: SIRAgent -> Bool
isInfected (Infected _) = True
isInfected _            = False

isRecovered :: SIRAgent -> Bool
isRecovered Recovered = True
isRecovered _         = False

initAgentsRand :: RandomGen g 
               => Double
               -> Int 
               -> Int 
               -> Rand g Agents
initAgentsRand illnessDuration n i = do
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