{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.MonadicStreamFunction

import SIR

type Time = Double
type DTime = Double

type EnvironmentFold e = [e] -> e -> e
type AgentMSF g s m e = MSF (ReaderT DTime (StateT (AgentOut s m) (Rand g))) (AgentIn m, e) e

type SIREnv = [AgentId]
type SIRAgentMSF g = AgentMSF g SIRState SIRMsg SIREnv

contactRate :: Double
contactRate = 5.0

infectionProb :: Double
infectionProb = 0.05

illnessDuration :: Double
illnessDuration = 15.0

agentCount :: Int
agentCount = 1000

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
  let fileName =  "SIR_DUNAI_AGENTIOSTATE_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g => g 
                      -> Time 
                      -> DTime
                      -> [SIRState]
                      -> [[SIRState]]
runSimulationUntil g t dt as = map (\(aos, _) -> map (fromJust. agentObservable) aos) aoss
  where
    steps = floor $ t / dt
    ticks = replicate steps ()

    msfs = map sirAgent as
    
    n = length as
    env = [0..n-1] 
    ains = map agentIn env
    
    aossM = embed (parSimulation msfs ains env sirEnvFold) ticks
    aoss = evalRand (evalStateT (runReaderT aossM dt) agentOut) g 

    sirEnvFold :: [SIREnv] -> SIREnv -> SIREnv
    sirEnvFold _ e = e

sirAgent :: RandomGen g => SIRState -> SIRAgentMSF g
sirAgent Susceptible  = susceptibleAgent
sirAgent Infected     = infectedAgent
sirAgent Recovered    = recoveredAgent

susceptibleAgent :: RandomGen g => SIRAgentMSF g
susceptibleAgent = switch susceptibleAgentInfectedEvent (const infectedAgent)
  where
    susceptibleAgentInfectedEvent :: RandomGen g => 
                                      MSF 
                                        (ReaderT DTime (StateT SIRAgentOut (Rand g)))
                                        (SIRAgentIn, SIREnv) 
                                        (SIREnv, Maybe ())
    susceptibleAgentInfectedEvent = proc (ain, e) -> do
        isInfected <- arrM (\ain' -> do 
          doInfect <- lift $ lift $ gotInfected infectionProb ain'
          if doInfect 
            then lift $ put (agentOutObs Infected) >> return doInfect
            else lift $ put (agentOutObs Susceptible) >> return doInfect) -< ain

        infEvt <- boolToMaybe () -< isInfected
        _ <- susceptibleAgentInfectedEventAux -< e
        returnA -< (e, infEvt)
      where
        susceptibleAgentInfectedEventAux :: RandomGen g => 
                                              MSF 
                                                (ReaderT DTime (StateT SIRAgentOut (Rand g)))
                                                (SIREnv) 
                                                ()
        susceptibleAgentInfectedEventAux = proc e -> do
          makeContact <- occasionally (1 / contactRate) () -< ()
          if isJust makeContact
            then arrM blub -< e
            else returnA -< ()

          where
            blub :: RandomGen g => 
                      SIREnv
                      -> ReaderT DTime (StateT SIRAgentOut (Rand g)) ()
            blub e = do
              randContact <- lift $ lift $ randomElem e
              lift $ sendMessageM (randContact, Contact Susceptible)
              return ()

boolToMaybe :: Monad m => a -> MSF m Bool (Maybe a)
boolToMaybe a = proc b -> do
  if b
    then returnA -< Just a
    else returnA -< Nothing

infectedAgent :: RandomGen g => SIRAgentMSF g
infectedAgent = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: RandomGen g => 
                                    MSF 
                                      (ReaderT DTime (StateT SIRAgentOut (Rand g)))
                                      (SIRAgentIn, SIREnv) 
                                      (SIREnv, Maybe ())
    infectedAgentRecoveredEvent = proc (ain, e) -> do
      recEvt <- occasionally illnessDuration () -< ()
      let a = maybe Infected (const Recovered) recEvt

      arrM (\(a, ain) -> do
        lift $ put (agentOutObs a)
        lift $ respondToContactWithM Infected ain) -< (a, ain)
        
      returnA -< (e, recEvt)

recoveredAgent :: RandomGen g => SIRAgentMSF g
recoveredAgent = proc (_, e) -> do
  arrM (\_ -> lift $ put (agentOutObs Recovered)) -< ()
  returnA -< e

parSimulation :: RandomGen g => 
                     [AgentMSF g s m e] 
                  -> [AgentIn m] 
                  -> e
                  -> EnvironmentFold e
                  -> MSF (ReaderT DTime (StateT (AgentOut s m) (Rand g)))
                      () 
                      ([AgentOut s m], e)
parSimulation msfs ains e ef = MSF $ \_ -> do
    aosMsfs <- mapM (parSimulationAux e) (zip ains msfs)

    let aoes = map fst aosMsfs
    let msfs' = map snd aosMsfs
    
    let aos = map fst aoes
    let es = map snd aoes

    let aids = map agentId ains
  
    let ains' = map (\ai -> agentIn $ agentId ai) ains 
    let ains'' = distributeMessages ains' (zip aids aos)

    let e' = ef es e

    return ((aos, e'), parSimulation msfs' ains'' e' ef)

  where
    parSimulationAux :: e
                        -> (AgentIn m, AgentMSF g s m e)
                        -> ReaderT DTime (StateT (AgentOut s m) (Rand g)) ((AgentOut s m, e), AgentMSF g s m e) 
    parSimulationAux e (ain, msf) = do
      _ <- lift $ put agentOut  -- NOTE: reset state
      (e', msf') <- unMSF msf (ain, e)
      ao <- lift $ get          -- NOTE: get state
      return ((ao, e'), msf')
    
-- NOTE: is in spirit of the Yampa implementation
occasionally :: RandomGen g => Time -> b -> MSF (ReaderT DTime (StateT (AgentOut s m) (Rand g))) a (Maybe b)
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