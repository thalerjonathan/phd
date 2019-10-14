module Utils.GenEventSIR where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.MonadicStreamFunction.InternalCore
import Test.Tasty.QuickCheck

import SIR.Event
import SIR.Model
import Utils.GenSIR

genEvent :: [AgentId] -> Gen SIREvent
genEvent = genEventFreq 1 1 1 (1,1,1)

genNonEmptyAgentIds :: Gen [AgentId]
genNonEmptyAgentIds = listOf1 (do 
  (Positive t) <- arbitrary :: Gen (Positive Int)
  return t)

genAgentIds :: Gen [AgentId]
genAgentIds = map (\(Positive i) -> i) <$> (arbitrary :: Gen [Positive Int])

genEventFreq :: Int
             -> Int
             -> Int
             -> (Int, Int, Int)
             -> [AgentId]
             -> Gen SIREvent
genEventFreq mcf _ rcf _ []  
  = frequency [ (mcf, return MakeContact), (rcf, return Recover)]
genEventFreq mcf cof rcf (s,i,r) ais
  = frequency [ (mcf, return MakeContact)
              , (cof, do
                  ss <- frequency [ (s, return Susceptible)
                                  , (i, return Infected)
                                  , (r, return Recovered)]
                  ai <- elements ais
                  return $ Contact ai ss)
              , (rcf, return Recover)]


genQueueItemStream :: Double 
                   -> [AgentId]
                   -> Gen [QueueItem SIREvent]
genQueueItemStream t ais = do
  evt  <- genQueueItem t ais
  evts <- genQueueItemStream (eventTime evt) ais
  return (evt : evts)

genQueueItem :: Double 
             -> [AgentId]
             -> Gen (QueueItem SIREvent)
genQueueItem t ais = do
  (Positive dt) <- arbitrary
  e <- genEvent ais
  receiver <- elements ais

  let evtTime = t + dt

  return $ QueueItem e receiver evtTime

eventTime :: QueueItem e -> Time
eventTime (QueueItem _ _ et) = et

genRunSusceptibleAgent :: Int
                       -> Double
                       -> Double
                       -> Double
                       -> [AgentId]
                       -> SIREvent
                       -> Gen (AgentId, SIRState, [QueueItem SIREvent])
genRunSusceptibleAgent cor inf ild t ais evt = do
  g <- genStdGen
  -- the susceptible agents id is picked randomly from all empty agent ids
  ai <- elements ais 
  -- create susceptible agent with agent id
  let a = susceptibleAgent ai cor inf ild
  -- run agent with given event and configuration
  let (_g', _a', ao, es) = runAgent g a evt t ais
  return (ai, ao, es)

genRunInfectedAgent :: Double
                    -> [AgentId]
                    -> SIREvent
                    -> Gen (AgentId, SIRState, [QueueItem SIREvent])
genRunInfectedAgent t ais evt = do
  g <- genStdGen
  -- the susceptible agents id is picked randomly from all empty agent ids
  ai <- elements ais 
  -- create susceptible agent with agent id
  let a = infectedAgent ai
  -- run agent with given event and configuration
  let (_g', _a', ao, es) = runAgent g a evt t ais
  return (ai, ao, es)

genRunRecoveredAgent :: Double
                     -> [AgentId]
                     -> SIREvent
                     -> Gen (SIRState, [QueueItem SIREvent])
genRunRecoveredAgent t ais evt = do
  g <- genStdGen
  -- create susceptible agent with agent id
  let a = recoveredAgent
  -- run agent with given event and configuration
  let (_g', _a', ao, es) = runAgent g a evt t ais
  return (ao, es)

genEventSIR :: [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Integer
            -> Double
            -> Gen [(Time, (Int, Int, Int))]
genEventSIR as cor inf ild maxEvents maxTime 
  = fst . runEventSIR as cor inf ild maxEvents maxTime <$> genStdGen

genLastEventSIR :: [SIRState]
                -> Int
                -> Double
                -> Double 
                -> Integer
                -> Double
                -> Gen (Time, (Int, Int, Int))
genLastEventSIR [] _ _ _ _ _ = return (0, (0,0,0))
genLastEventSIR as cor inf ild maxEvents maxTime = do
  ret <- genEventSIR as cor inf ild maxEvents maxTime
  if null ret
    then return (maxTime, aggregateSIRStates as)
    else return (last ret)
    
genEventSIRRepls :: Int 
                 -> [SIRState]
                 -> Int
                 -> Double
                 -> Double 
                 -> Integer
                 -> Double
                 -> Gen [(Int, Int, Int)]
genEventSIRRepls n as cor inf ild maxEvents tMax
  = map snd <$> vectorOf n (genLastEventSIR as cor inf ild maxEvents tMax)

--------------------------------------------------------------------------------
-- AGENT RUNNER
--------------------------------------------------------------------------------
runAgent :: RandomGen g
         => g
         -> SIRAgentMSF g
         -> SIREvent
         -> Time
         -> [AgentId]
         -> (g, SIRAgentMSF g, SIRState, [QueueItem SIREvent])
runAgent g a e t ais  = (g', a', ao, es)
  where
    aMsf       = unMSF a e
    aEvtWriter = runReaderT aMsf t
    aAisReader = runWriterT aEvtWriter
    aRand      = runReaderT aAisReader ais

    (((ao, a'), es), g') = runRand aRand g
