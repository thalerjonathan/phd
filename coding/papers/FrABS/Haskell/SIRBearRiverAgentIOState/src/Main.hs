{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.IO
import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import FRP.BearRiver

import SIR

type EnvironmentFold e  = [e] -> e -> e
type AgentMSF g s m e   = SF (StateT (AgentOut s m) (Rand g)) (AgentIn m, e) e

type SIREnv         = [AgentId]
type SIRAgentMSF g  = AgentMSF g SIRState SIRMsg SIREnv

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

globalDt :: DTime
globalDt = 0.1

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  
  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t globalDt as

  let dyns = aggregateAllStates ass
  let fileName =  "SIR_BEARRIVER_AGENTIOSTATE_DYNAMICS_" ++ show agentCount ++ "agents.m"
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
                                      SF 
                                        (StateT SIRAgentOut (Rand g))
                                        (SIRAgentIn, SIREnv) 
                                        (SIREnv, Event ())
    susceptibleAgentInfectedEvent = proc (ain, e) -> do
        isInfected <- arrM (\ain' -> do 
          doInfect <- lift $ lift $ gotInfected infectionProb ain'
          if doInfect 
            then lift $ put (agentOutObs Infected) >> return doInfect
            else lift $ put (agentOutObs Susceptible) >> return doInfect) -< ain

        infEvt <- edge -< isInfected
        _ <- susceptibleAgentInfectedEventAux -< e
        returnA -< (e, infEvt)
      where
        susceptibleAgentInfectedEventAux :: RandomGen g => 
                                              SF 
                                                (StateT SIRAgentOut (Rand g))
                                                (SIREnv) 
                                                ()
        susceptibleAgentInfectedEventAux = proc e -> do
          makeContact <- occasionally (1 / contactRate) () -< ()
          if isEvent makeContact
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

infectedAgent :: RandomGen g => SIRAgentMSF g
infectedAgent = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: RandomGen g => 
                                    SF 
                                      (StateT SIRAgentOut (Rand g))
                                      (SIRAgentIn, SIREnv) 
                                      (SIREnv, Event ())
    infectedAgentRecoveredEvent = proc (ain, e) -> do
      recEvt <- occasionally illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt

      arrM (\(a, ain) -> do
        lift $ put (agentOutObs a)
        lift $ respondToContactWithM Infected ain) -< (a, ain)
        
      returnA -< (e, recEvt)

recoveredAgent :: RandomGen g => SIRAgentMSF g
recoveredAgent = proc (_, e) -> do
  arrM_ $ lift $ put (agentOutObs Recovered) -< ()
  returnA -< e

  
parSimulation :: RandomGen g => 
                     [AgentMSF g s m e] 
                  -> [AgentIn m] 
                  -> e
                  -> EnvironmentFold e
                  -> SF (StateT (AgentOut s m) (Rand g))
                      () 
                      ([AgentOut s m], e)
parSimulation msfs0 ains0 e0 ef = loopPre (msfs0, ains0, e0) (parSimulationAux ef)
  where
    parSimulationAux :: EnvironmentFold e
                        -> SF (StateT (AgentOut s m) (Rand g)) 
                          ((), ([AgentMSF g s m e], [AgentIn m], e))
                          (([AgentOut s m], e), ([AgentMSF g s m e], [AgentIn m], e))
    parSimulationAux ef = proc (_, (msfs, ains, e)) -> do
      aosMsfs <- arrM (\(msfs, ains, e) -> mapM (runAgent e) (zip ains msfs)) -< (msfs, ains, e)
  
      let aoes = map fst aosMsfs
      let msfs' = map snd aosMsfs
      
      let aos = map fst aoes
      let es = map snd aoes
  
      let aids = map agentId ains
    
      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeMessages ains' (zip aids aos)
  
      let e' = ef es e
  
      returnA -< ((aos, e'), (msfs', ains'', e'))

    runAgent :: e
              -> (AgentIn m, AgentMSF g s m e)
              -> ReaderT DTime (StateT (AgentOut s m) (Rand g)) 
                  ((AgentOut s m, e), AgentMSF g s m e) 
    runAgent e (ain, msf) = do
      _ <- lift $ put agentOut  -- NOTE: reset state
      (e', msf') <- unMSF msf (ain, e)
      ao <- lift $ get          -- NOTE: get state
      return ((ao, e'), msf')

-- NOTE: is in spirit of the Yampa implementation
-- NAIVE IMPLEMENTATION
occasionallyNaive :: (RandomGen g, Monad m) => g -> Time -> b -> SF m a (Event b)
occasionallyNaive g t_avg b
    | t_avg > 0 = MSF (const $ tf g b)
    | otherwise = error "AFRP: occasionally: Non-positive average interval."
  where
    tf :: (RandomGen g, Monad m) => g -> b -> ClockInfo m (Event b, SF m a (Event b))
    tf g b = do
      dt <- ask
      let (r, g') = randomR (0, 1) g
      let p = 1 - exp (-(dt / t_avg))
      let evt = if r < p 
                  then Event b 
                  else NoEvent
      return (evt, MSF (const $ tf g' b))

-- is avoiding MSF constructor
occasionallyNaiveFeedback :: (RandomGen g, Monad m) => g -> Time -> b -> SF m a (Event b)
occasionallyNaiveFeedback g t_avg b
    | t_avg > 0 = proc _ -> do
      r <- getRandomS g -< ()
      dt <- timeDelta -< ()
      let p = 1 - exp (-(dt / t_avg))
      if r < p
        then returnA -< Event b
        else returnA -< NoEvent
    | otherwise = error "AFRP: occasionally: Non-positive average interval."

timeDelta :: Monad m => SF m a DTime
timeDelta = arrM_ ask

getRandomS :: (RandomGen g, Random b, Monad m) => g -> SF m a b
getRandomS g0 = feedback g0 getRandomSAux
  where
    getRandomSAux = proc (_, g) -> do
      let (r, g') = random g
      returnA -< (r, g')

-- NOTE: is in spirit of MSFs
-- hard-coded monad-stack, not very flexible
occasionallyMSF :: RandomGen g => Time -> b -> SF (StateT (AgentOut s m) (Rand g)) a (Event b)
occasionallyMSF t_avg b
  | t_avg > 0 = proc _ -> do
    r <- arrM_ $ getRandomR (0, 1) -< ()
    dt <- timeDelta -< ()
    let p = 1 - exp (-(dt / t_avg))
    if r < p
      then returnA -< Event b
      else returnA -< NoEvent
  | otherwise = error "AFRP: occasionally: Non-positive average interval."

-- | Updates the generator every step
-- Hint: Use the isomorphism 'RandT ~ StateT' and then 'Control.Monad.Trans.MSF.State'
runRandS :: (RandomGen g, Monad m) => MSF (RandT g m) a b -> g -> MSF m a (g, b)
runRandS msf g = MSF $ \a -> do
  ((b, msf'), g') <- runRandT (unMSF msf a) g
  return ((g', b), runRandS msf' g')

evalRandS  :: (RandomGen g, Monad m) => MSF (RandT g m) a b -> g -> MSF m a b
evalRandS msf g = runRandS msf g >>> arr snd

-- NOTE: final version, this is what we want as it can be used in any transformer stack with a Rand in it
occasionally :: MonadRandom m => Time -> b -> SF m a (Event b)
occasionally t_avg b
  | t_avg > 0 = proc _ -> do
    r <- getRandomRS (0, 1) -< ()
    dt <- timeDelta -< ()
    let p = 1 - exp (-(dt / t_avg))
    if r < p
      then returnA -< Event b
      else returnA -< NoEvent
  | otherwise = error "AFRP: occasionally: Non-positive average interval."

getRandomRS :: (MonadRandom m, Random b) => (b, b) -> SF m a b
getRandomRS r = proc _ -> do
  r <- arrM_ $ getRandomR r -< ()
  returnA -< r 