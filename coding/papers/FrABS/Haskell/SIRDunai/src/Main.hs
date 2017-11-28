{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Data.List
import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
--import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction

import SIR

type Time = Double
type DTime = Double

type SIRAgentMSF g = MSF (ReaderT DTime (Rand g)) () SIRState

contactRate :: Double
contactRate = 5.0

infectionProb :: Double
infectionProb = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentCount :: Int
agentCount = 100

infectedCount :: Int
infectedCount = 10

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.01

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  
  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "SIR_DUNAI_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g => g -> Time -> DTime -> [SIRState] -> [[SIRState]]
runSimulationUntil g t dt as = evalRand (runReaderT ass dt) g -- runReader (evalRand ass g) dt -- evalRand (runReader ass dt) g 
  where
    steps = floor $ t / dt
    ticks = replicate steps ()
    msfs = map sirAgent as
    ass = embed (sirSimulation msfs as) ticks

sirSimulation :: RandomGen g => [SIRAgentMSF g] -> [SIRState] -> MSF (ReaderT DTime (Rand g)) () [SIRState]
sirSimulation msfs _as = MSF $ \_a -> do
    (as', msfs') <- foldM sirSimulationAux ([], []) msfs
    return (as', sirSimulation msfs' as')
  where
    sirSimulationAux :: ([SIRState], [SIRAgentMSF g])
                        -> SIRAgentMSF g 
                        -> ReaderT DTime (Rand g) ([SIRState], [SIRAgentMSF g])
    sirSimulationAux (accStates, accSfs) sf = do
      (s, sf') <- unMSF sf ()
      return (s : accStates, sf' : accSfs)

sirAgent :: RandomGen g => SIRState -> SIRAgentMSF g
sirAgent Susceptible  = susceptibleAgent
sirAgent Infected     = infectedAgent
sirAgent Recovered    = recoveredAgent

susceptibleAgent :: RandomGen g => SIRAgentMSF g
susceptibleAgent = switch susceptibleAgentInfectedEvent (const infectedAgent)
  where
    susceptibleAgentInfectedEvent :: RandomGen g => MSF (ReaderT DTime (Rand g)) () (SIRState, Maybe ())
    susceptibleAgentInfectedEvent = arrM (const susceptibleAgentInfectedEventAux)
      where
        susceptibleAgentInfectedEventAux :: RandomGen g => ReaderT DTime (Rand g) (SIRState, Maybe ())
        susceptibleAgentInfectedEventAux = do
          let as = []
          randContactCount <- lift $ randomExpM (1 / contactRate)
          aInfs <- lift $ doTimes (floor randContactCount) (susceptibleAgentAux as) -- TODO: replace by messaging
          let mayInf = find (Infected==) aInfs
          if isJust mayInf
            then return (Susceptible, Just ())
            else return (Infected, Nothing)

        susceptibleAgentAux :: RandomGen g => [SIRState] -> Rand g SIRState
        susceptibleAgentAux as = do
          randContact <- randomElem as
          if (Infected == randContact)
            then infect
            else return Susceptible
    
        infect :: RandomGen g => Rand g SIRState
        infect = do
          doInfect <- randomBoolM infectionProb
          if doInfect
            then return Infected
            else return Susceptible

infectedAgent :: RandomGen g => SIRAgentMSF g
infectedAgent = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: RandomGen g => MSF (ReaderT DTime (Rand g)) () (SIRState, Maybe ())
    infectedAgentRecoveredEvent = proc _ -> do
      recEvt <- occasionally illnessDuration () -< ()
      let a = maybe Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

recoveredAgent :: RandomGen g => SIRAgentMSF g
recoveredAgent = arr (const Recovered)

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

doTimes :: (Monad m) => Int -> m a -> m [a]
doTimes n f = forM [0..n - 1] (\_ -> f) 

-- NOTE: is in spirit of the Yampa implementation
occasionally :: RandomGen g => Time -> b -> MSF (ReaderT DTime (Rand g)) a (Maybe b)
occasionally t_avg b
    | t_avg > 0 = MSF (const tf)
    | otherwise = error "AFRP: occasionally: Non-positive average interval."
  where
    -- Generally, if events occur with an average frequency of f, the
    -- probability of at least one event occurring in an interval of t
    -- is given by (1 - exp (-f*t)). The goal in the following is to
    -- decide whether at least one event occurred in the interval of size
    -- dt preceding the current sample point. For the first point,
    -- we can think of the preceding interval as being 0, implying
    -- no probability of an event occurring.

    tf = do
      dt <- ask
      r <- lift $ getRandomR (0, 1)
      let p = 1 - exp (-(dt / t_avg))
      let evt = if r < p 
                  then Just b 
                  else Nothing
      return (evt, MSF (const tf))