module Utils.GenEventSIR where

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

  return $ QueueItem receiver (Event e) evtTime

eventTime :: QueueItem e -> Time
eventTime (QueueItem _ _ et) = et

genEventSIR :: [SIRState]
            -> Int
            -> Double
            -> Double 
            -> Integer
            -> Double
            -> Gen [(Time, (Int, Int, Int))]
genEventSIR as cor inf ild maxEvents maxTime 
  = fst . runSIR as cor inf ild maxEvents maxTime <$> genStdGen
