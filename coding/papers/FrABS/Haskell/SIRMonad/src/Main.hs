module Main where

import Data.List
import Data.Maybe

import Control.Monad.Random

import SIR

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

  as <- evalRandIO $ initAgentsRand agentCount infectedCount
  -- as' <- evalRandIO $ runSimulationUntil t dt as
  (tEnd, ass) <- evalRandIO $ runSimulation dt as
  
  putStrLn $ "All Recovered after t = " ++ show tEnd
  
  let dyns = aggregateAllStates (map (\as' -> map fst as') ass)
  let fileName =  "SIR_MONAD_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g 
                   => Time 
                   -> TimeDelta 
                   -> Agents 
                   -> Rand g [Agents]
runSimulationUntil tEnd dt as = runSimulationUntilAux tEnd 0 dt as []
  where
    runSimulationUntilAux :: RandomGen g 
                          => Time 
                          -> Time 
                          -> TimeDelta 
                          -> Agents 
                          -> [Agents] 
                          -> Rand g [Agents]
    runSimulationUntilAux tEnd t dt as acc
      | t >= tEnd = return $ reverse (as : acc)
      | otherwise = do
        as' <- stepSimulation dt as 
        runSimulationUntilAux tEnd (t + dt) dt as' (as : acc)

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
      | noInfected as = return $ (t, reverse $ as : acc)
      | otherwise = do
          as' <- stepSimulation dt as 
          runSimulationAux (t + dt) dt as' (as : acc)

    noInfected :: Agents -> Bool
    noInfected as = not $ any (is Infected) as

stepSimulation :: RandomGen g => TimeDelta -> Agents -> Rand g Agents
stepSimulation dt as = mapM (processAgent dt as) as

processAgent :: RandomGen g 
             => TimeDelta 
             -> Agents 
             -> SIRAgent 
             -> Rand g SIRAgent
processAgent _ as (Susceptible, _) = susceptibleAgent as
processAgent dt _ a@(Infected, _) = return $ infectedAgent dt a
processAgent _ _ a@(Recovered, _) = return a

-- NOTE: does not exclude contact with itself but with a sufficiently large number of agents the probability becomes very small
susceptibleAgent :: RandomGen g => Agents -> Rand g SIRAgent
susceptibleAgent as = do
    randContactCount <- randomExpM (1 / contactRate)
    aInfs <- doTimes (floor randContactCount) (susceptibleAgentAux as)
    let mayInf = find (is Infected) aInfs
    return $ fromMaybe susceptible mayInf

  where
    susceptibleAgentAux :: RandomGen g => Agents -> Rand g SIRAgent
    susceptibleAgentAux as = do
      randContact <- randomElem as
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

is :: SIRState -> SIRAgent -> Bool
is s (s',_) = s == s'

initAgentsRand :: RandomGen g => Int -> Int -> Rand g Agents
initAgentsRand n i = do
  let sus = replicate (n - i) susceptible
  expTs <- mapM randomExpM (replicate i (1 / illnessDuration))
  let inf = map infected expTs
  return $ sus ++ inf