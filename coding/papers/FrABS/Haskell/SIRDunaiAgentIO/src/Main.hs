{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Data.Maybe

import Control.Monad.Random
import Control.Monad.Reader
import Data.MonadicStreamFunction

import SIR

type Time = Double
type DTime = Double

type EnvironmentFold e = [e] -> e -> e
type AgentMSF g s m e = MSF (ReaderT DTime (Rand g)) (AgentIn m, e) (AgentOut s m, e)

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
dt = 0.1

t :: Time
t = 150

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  
  let as = initAgents agentCount infectedCount
  let ass = runSimulationUntil g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "SIR_DUNAI_AGENTIO_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulationUntil :: RandomGen g => g 
                      -> Time 
                      -> DTime
                      -> [SIRState]
                      -> [[SIRState]]
runSimulationUntil g t dt as = map (\(aos, _) -> map (fromJust . agentObservable) aos) aoss
  where
    steps = floor $ t / dt
    ticks = replicate steps ()

    msfs = map sirAgent as
    
    n = length as
    env = [0..n-1] 
    ains = map agentIn env
    
    aossM = embed (parSimulation msfs ains env sirEnvFold) ticks
    aoss = evalRand (runReaderT aossM dt) g 

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
                                        (ReaderT DTime (Rand g)) 
                                        (SIRAgentIn, SIREnv) 
                                        ((SIRAgentOut, SIREnv), Maybe ())
    susceptibleAgentInfectedEvent = proc (ain, e) -> do
      isInfected <- arrM (\ain' -> do 
        flag <- lift $ gotInfected ain'
        return flag) -< ain
      let (ao, infEvt) = if isInfected 
                          then (agentOutObs Infected, Just ()) 
                          else (agentOutObs Susceptible, Nothing)

      ao' <- susceptibleAgentInfectedEventAux -< (ao, e)
      returnA -< ((ao', e), infEvt)

      where
        susceptibleAgentInfectedEventAux :: RandomGen g => 
                                              MSF 
                                                (ReaderT DTime (Rand g)) 
                                                (SIRAgentOut, SIREnv) 
                                                (SIRAgentOut)
        susceptibleAgentInfectedEventAux = proc (ao, e) -> do
          makeContact <- occasionally (1 / contactRate) () -< ()
          if isJust makeContact
            then arrM (\(ao', e) -> do
              randContact <- lift $ randomElem e
              return (sendMessage (randContact, Contact Susceptible) ao')) -< (ao, e)
            else returnA -< ao

infectedAgent :: RandomGen g => SIRAgentMSF g
infectedAgent = switch infectedAgentRecoveredEvent (const recoveredAgent)
  where
    infectedAgentRecoveredEvent :: RandomGen g => 
                                    MSF 
                                      (ReaderT DTime (Rand g)) 
                                      (SIRAgentIn, SIREnv) 
                                      ((SIRAgentOut, SIREnv), Maybe ())
    infectedAgentRecoveredEvent = proc (ain, e) -> do
      recEvt <- occasionally illnessDuration () -< ()
      let a = maybe Infected (const Recovered) recEvt
      let ao = agentOutObs a
      let ao' = respondToContactWith Infected ain ao
      returnA -< ((ao', e), recEvt)

recoveredAgent :: RandomGen g => SIRAgentMSF g
recoveredAgent = first $ arr (const (agentOutObs Recovered))

gotInfected :: RandomGen g => SIRAgentIn -> Rand g Bool
gotInfected ain = onMessageM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentMessage SIRMsg -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectionProb
    gotInfectedAux x _ = return x

respondToContactWith :: SIRState -> SIRAgentIn -> SIRAgentOut -> SIRAgentOut
respondToContactWith state ain ao = onMessage respondToContactWithAux ain ao
  where
    respondToContactWithAux :: AgentMessage SIRMsg -> SIRAgentOut -> SIRAgentOut
    respondToContactWithAux (senderId, Contact _) ao = sendMessage (senderId, Contact state) ao


parSimulation :: RandomGen g => 
                     [AgentMSF g s m e] 
                  -> [AgentIn m] 
                  -> e
                  -> EnvironmentFold e
                  -> MSF (ReaderT DTime (Rand g)) 
                      () 
                      ([AgentOut s m], e)
parSimulation msfs ains e ef = MSF $ \_ -> do
    (aoe, msfs') <- foldM (parSimulationAux e) ([], []) (zip msfs ains)

    let aos = map fst aoe
    let es = map snd aoe

    let aids = map agentId ains
  
    let ains' = map (agentIn . agentId) ains 
    let ains'' = distributeMessages ains' (zip aids aos)

    let e' = ef es e

    return ((aos, e'), parSimulation msfs' ains'' e' ef)

  where
    parSimulationAux :: e
                        -> ([(AgentOut s m, e)], [AgentMSF g s m e])
                        -> (AgentMSF g s m e, AgentIn m)
                        -> ReaderT DTime (Rand g) 
                            ([(AgentOut s m, e)], [AgentMSF g s m e])
    parSimulationAux e (accOutEnv, accSfs) (sf, ain) = do
      (aoe, sf') <- unMSF sf (ain, e)
      return (aoe : accOutEnv, sf' : accSfs)

      {-
onMessageM :: (Monad mon) => (acc -> AgentMessage m -> mon acc) -> AgentIn m -> acc -> mon acc
onMessageM msgHdl ai acc
    | null msgs = return acc
    | otherwise = foldM msgHdl acc msgs
  where
    msgs = aiMsgs ai

onMessage :: (AgentMessage m -> acc -> acc) -> AgentIn m -> acc -> acc
onMessage msgHdl ai a 
    | null msgs = a
    | otherwise = foldr (\msg acc'-> msgHdl msg acc') a msgs
  where
    msgs = aiMsgs ai
    -}

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