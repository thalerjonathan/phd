module SIRGenerators where

import Control.Monad.Random
import Test.Tasty.QuickCheck as QC

import SIR.SIR

genEvent :: [AgentId] -> Gen SIREvent
genEvent = genEventFreq 1 1 1 (1,1,1)

genStdGen :: Gen StdGen
genStdGen = do
  seed <- choose (minBound, maxBound)
  return $ mkStdGen seed

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

genSIRState :: Gen SIRState
genSIRState = elements [Susceptible, Infected, Recovered]

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

  return $ QueueItem receiver (Event e) evtTime

eventTime :: QueueItem e -> Time
eventTime (QueueItem _ _ et) = et

genSimulationSIR :: [SIRState]
                 -> Int
                 -> Double
                 -> Double 
                 -> Integer
                 -> Double
                 -> Gen [(Time, (Int, Int, Int))]
genSimulationSIR ss cr inf illDur maxEvents maxTime 
  = fst . runSIR ss cr inf illDur maxEvents maxTime <$> genStdGen
