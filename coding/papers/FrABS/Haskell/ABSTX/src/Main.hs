{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Maybe
import qualified Data.Map as Map
import System.IO
--import Debug.Trace
import Data.List

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Reader
import FRP.BearRiver

type AgentId = Int
type DataFlow d = (AgentId, d)

data AgentIn d = AgentIn
  {
    aiId        :: AgentId
  , aiData      :: [DataFlow d]
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o

  , aoFreezeTime  :: Bool
  } deriving (Show)

type Agent m o d          = SF m (AgentIn d) (AgentOut o d)
type AgentObservable o    = (AgentId, Maybe o)

type ConvTestObservable   = Double

data ConvTestData         = Offering Double
                          | OfferingRefuse 
                          | OfferingAccept
                          deriving (Show, Eq)

type ConvTestAgentIn        = AgentIn ConvTestData
type ConvTestAgentOut       = AgentOut ConvTestObservable ConvTestData
type ConvTestAgent g        = Agent (RandT g IO) ConvTestObservable ConvTestData

type ConvTestEnv            = [AgentId]

agentCount :: Int
agentCount = 2

rngSeed :: Int
rngSeed = 42

t :: Time
t = 10

timeDelta :: DTime
timeDelta = 1.0

initAgentWealth :: Double
initAgentWealth = 100

-- FAZIT SO FAR: problem is that one now needs to match the sender against the TX initiator
--               but this could be hidden away with a proper TX EDSL.
--               Is obviously slower
--               Cumbersomeness of state-changes does not disappear but we need that 
--               functionality for our agent-library anyway so if we come up with a proper 
--               implementation of such state-machines?
--
--               the benefit of the other approach was that when we were engaged in a TX
--               we wouldn't care about the receiver/sender, it would be routed automatically
--               
--               => this attempt is a fail, it is just even more cumbersome to use for the user
--                  except for the easy "freezeTime" feature. Follow the ABSConv approach.

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initTestAgents
  obss <- runSimulationUntil g t timeDelta as
  mapM_ (\(t, obs) -> putStrLn ("\nt = " ++ show t) >> mapM_ (putStrLn . show) obs) obss

{-
  let as = map (\aid -> (aid, timeZeroAgent)) [0..agentCount - 1]
  obss <- runSimulationUntil g t timeDelta as 
  mapM_ (\(t, obs) -> putStrLn ("\nt = " ++ show t) >> mapM_ (putStrLn . show) obs) obss

timeZeroAgent :: RandomGen g => ConvTestAgent g
timeZeroAgent = proc _ -> do
  t <- time -< ()
  printDebugS -< ("timeZeroAgent t = " ++ show t)

  rec
    s' <- iPre (1 :: Int) -< s
    let s = if s' + 1 == 10 then 0 else s' + 1

  printDebugS -< ("timeZeroAgent s = " ++ show s)

  if s' == 0 
    then returnA -< agentOutObs t
    else returnA -< freezeTime $ agentOutObs t
-}

initTestAgents :: RandomGen g 
               => [(AgentId, ConvTestAgent g)]
initTestAgents = [aa, pa] 
  where
    aa = (0, activeAgent 100 [1])
    pa = (1, passiveAgent 100 [0])

-------------------------------------------------------------------------------
-- AGENTS
-------------------------------------------------------------------------------
activeAgent :: RandomGen g 
                => Double
                -> ConvTestEnv
                -> ConvTestAgent g
activeAgent w0 env = proc ain -> do
  evt <- after 1.0 () -< ()
  if isEvent evt
    then activeTxAgentInit w0 env -< ain
    else returnA -< agentOutObs w0

passiveAgent :: RandomGen g 
                 => Double
                 -> ConvTestEnv
                 -> ConvTestAgent g
passiveAgent w0 env = 
    dSwitch 
      checkTxAgent 
      (\bid -> passiveTxAgentAwait w0 env bid)
  where
    checkTxAgent :: RandomGen g
                  => SF (RandT g IO)
                      ConvTestAgentIn 
                      (ConvTestAgentOut, Event Double)
    checkTxAgent = proc ain -> do
      t <- time -< ()
      printDebugS -< ("passiveTx: t = " ++ show t)

      let mayOffering = find (\(senderId, d) -> isOffering d) (aiData ain)

      if isJust mayOffering
        then (do
          let tx = fromJust $ mayOffering
          printDebugS -< ("passiveTX: begin incoming TX = " ++ show tx)
          ret <- passiveTxAgentInit w0 env -< tx
          returnA -< ret)
        else (do
          printDebugS -< ("passiveTX: awaiting incoming TX")
          returnA -< (agentOut, NoEvent)) -- output does not matter at this point

-------------------------------------------------------------------------------
-- PASSIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
passiveTxAgentInit :: RandomGen g 
                   => Double
                   -> ConvTestEnv
                   -> SF (RandT g IO)
                        (DataFlow ConvTestData) 
                        (ConvTestAgentOut, Event Double)
passiveTxAgentInit w env = proc (senderId, d) -> 
  if isOffering d 
    then passiveTxAgentReply w -< (senderId, offeringValue d)
    else (do
      printDebugS -< ("passiveTX: invalid protocoll, abort TX")
      returnA -< (agentOut, NoEvent)) -- Invalid protocoll, abort TX

isOffering :: ConvTestData -> Bool
isOffering (Offering _) = True
isOffering _ = False

offeringValue :: ConvTestData -> Double
offeringValue (Offering o) = o
offeringValue _ = error "not an Offering"

passiveTxAgentReply :: RandomGen g 
                    => Double
                    -> SF (RandT g IO)
                        (AgentId, Double)
                        (ConvTestAgentOut, Event Double)
passiveTxAgentReply w = proc (senderId, v) -> do
  printDebugS -< ("passiveTX: received Offering = " ++ show v)

  printDebugS -< ("passiveTX: checking budget constraint, ask = " ++ show v ++ ", wealth = " ++ show w)
  if v >= w
    then (do
      printDebugS -< ("passiveTX: not enough budget, refusing offer")
      let ao = freezeTime $ dataFlow (senderId, OfferingRefuse) (agentOutObs w)
      returnA -< (ao, NoEvent))
    else (do
      bid <- arrM_ (getRandomR (0, w)) -< ()
      printDebugS -< ("passiveTX: enough budget, accepting offering, my Offering is " ++ show bid)
      returnA -< (freezeTime $ dataFlow (senderId, Offering bid) agentOut, Event bid))

passiveTxAgentAwait :: RandomGen g
                    => Double
                    -> ConvTestEnv
                    -> Double
                    -> ConvTestAgent g
passiveTxAgentAwait w env bid =  
    dSwitch
      --((agentOutObs w, noEvent) --> checkPassiveTxAgentAwait)
      (checkPassiveTxAgentAwait)
      (\w' -> passiveAgent w' env)
  where
    checkPassiveTxAgentAwait :: RandomGen g
                             => SF (RandT g IO) 
                                  ConvTestAgentIn 
                                  (ConvTestAgentOut, Event Double)
    checkPassiveTxAgentAwait = proc ain -> do
      printDebugS -< ("passiveTX: waiting for reply to my offering...")
      if hasTxData ain 
        then (do
          let d = txDataIn ain
          printDebugS -< ("passiveTX: received reply = " ++ show d)
          returnA -< handleReply d w bid)
        else (do
          printDebugS -< ("passiveTX: no reply yet!")
          returnA -< (agentOut, NoEvent))

    handleReply :: ConvTestData
                -> Double
                -> Double
                -> (ConvTestAgentOut, Event Double)
    -- active agent refuses, no exchange but commit TX
    handleReply OfferingRefuse w _  = trace ("passiveTX: OfferingRefuse, commit") (commitTx $ agentOutObs w, Event w) 
    -- active agent accepts, make exchange and commit TX
    handleReply OfferingAccept w bid = trace ("passiveTX: OfferingAccept, commit") (commitTx $ agentOutObs $ w - bid, Event $ w - bid) 
    -- abort because wrong protocoll 
    handleReply _              _  _ = trace ("passiveTX: protocoll fault, abort") (abortTx agentOut, NoEvent) 

-------------------------------------------------------------------------------
-- ACTIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
activeTxAgentInit :: RandomGen g 
                  => Double
                  -> ConvTestEnv
                  -> ConvTestAgent g
activeTxAgentInit w env = 
  dSwitch -- VERY IMPORTANT TO USE DELAY HERE OTHERWISE activeTxAgentAwait output would override!
    --((agentOutObs w, noEvent) --> activeTxAgentBegin w env)
    (activeTxAgentBegin w env)
    (\ask -> activeTxAgentAwait w env ask)

activeTxAgentBegin :: RandomGen g 
                   => Double
                   -> ConvTestEnv
                   -> SF (RandT g IO) ConvTestAgentIn (ConvTestAgentOut, Event Double)
activeTxAgentBegin w env = proc ain -> do
  let aid = agentId ain
  ridx <- arrM_ (getRandomR (0, length env - 1)) -< ()
  let raid = env !! ridx
  
  t <- time -< ()
  printDebugS -< ("activeTX: t = " ++ show t)
  printDebugS -< ("activeTX: drawing random agentid = " ++ show raid)
  
  if aid == raid
    then activeTxAgentBegin w env -< ain
    else (do
      ask <- arrM_ (getRandomR (0, w)) -< ()
      let d = (raid, Offering ask)
      printDebugS -< ("activeTX: beginTx = " ++ show d)
      returnA -< (beginTx d agentOut, Event ask))

activeTxAgentAwait :: RandomGen g
                   => Double
                   -> ConvTestEnv
                   -> Double
                   -> ConvTestAgent g
activeTxAgentAwait w env ask = 
    dSwitch
      ((agentOutObs w, noEvent) --> checkActiveTxAgentAwait)
      --(checkActiveTxAgentAwait)
      (\w' -> activeAgent w' env)
  where
    checkActiveTxAgentAwait :: RandomGen g
                            => SF (RandT g IO) 
                                  ConvTestAgentIn 
                                  (ConvTestAgentOut, Event Double)
    checkActiveTxAgentAwait = proc ain -> do
      if hasTxData ain 
        then (do
          printDebugS -< ("activeTX: received tx reply = " ++ show (txDataIn ain))
          returnA -< handleReply (txDataIn ain) w ask)
        else (do
          printDebugS -< ("activeTX: tx reply not yet received")
          returnA -< (agentOut, NoEvent)) -- output does not matter

    handleReply :: ConvTestData
                -> Double
                -> Double
                -> (ConvTestAgentOut, Event Double)
    -- passive agent refuses, no exchange but commit TX
    handleReply OfferingRefuse w _    = (commitTx agentOut, Event w)
    -- passive agent never replies with OfferingAccept
    handleReply OfferingAccept w ask = (abortTx agentOut, Event w)
    handleReply (Offering bid) w ask 
      | bid <= ask = trace ("activeTX: crossover, commit") (commitTx $ txDataOut OfferingAccept (agentOutObs $ w + ask),  Event $ w + ask)
      | otherwise = trace ("activeTX: no crossover, commit") (commitTx $ txDataOut OfferingRefuse (agentOutObs w), Event w)

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
runSimulationUntil :: (RandomGen g, Show o, Show d)
                   => g
                   -> Time 
                   -> DTime
                   -> [(AgentId, Agent (RandT g IO) o d)]
                   -> IO [(Time, [AgentObservable o])]
runSimulationUntil g t dt aiMsfs = do
  let steps = floor $ t / dt
  let ticks = replicate steps ()

  let ais = map fst aiMsfs
  let msfs = map snd aiMsfs
  let ains  = map agentIn ais

  let aossM = embed (parSimulation msfs ains) ticks

  let readerM = runReaderT aossM dt
  aoss <- evalRandT readerM g

  let aobs = map (\(t, aos) -> (t, map (\(aid, ao) -> (aid, agentObservable ao)) aos)) aoss

  return aobs

parSimulation :: (RandomGen g, Show o, Show d)
              => [Agent (RandT g IO) o d] 
              -> [AgentIn d] 
              -> SF (RandT g IO)
                  ()
                  (Time, [(AgentId, AgentOut o d)])
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: (RandomGen g, Show o, Show d)
                     => SF (RandT g IO)
                          ((), ([Agent (RandT g IO) o d], [AgentIn d]))
                          ((Time, [(AgentId, AgentOut o d)]), ([Agent (RandT g IO) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      (msfs', aios, ains') <- runAgentsAux -< (False, msfs, ains)
      t                    <- time      -< ()

      returnA -< ((t, aios), (msfs', ains'))

runAgents :: RandomGen g
          => SF (RandT g IO)
              (Bool, [Agent (RandT g IO) o d], [AgentIn d]) 
              ([Agent (RandT g IO) o d], [(AgentId, AgentOut o d)], [AgentIn d])
runAgents = readerS $ proc (dt, (freezeTime, sfs, ains)) -> do
    let dt' = if freezeTime then 0 else dt

    let asIns = zipWith (\sf ain -> (dt', (ain, sf))) sfs ains
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets

    let aios = map (\(ao, ai) -> (agentId ai, ao)) (zip aos ains)
    let ains' = map (\ai -> agentIn $ agentId ai) ains 
    let ains'' = distributeData ains' aios

    returnA -< (sfs', aios, ains'')

runAgentsAux :: RandomGen g
             => SF (RandT g IO)
                (Bool, [Agent (RandT g IO) o d], [AgentIn d]) 
                ([Agent (RandT g IO) o d], [(AgentId, AgentOut o d)], [AgentIn d])
runAgentsAux = proc (flag, sfs, ains) -> do
  (sfs', aios, ains') <- runAgents -< (flag, sfs, ains)

  if any (isFreezeTime . snd) aios
    then runAgentsAux -< (True, sfs', ains')
    else returnA -< (sfs', aios, ains')

runAgent :: RandomGen g
         => SF (RandT g IO)
              (AgentIn d, Agent (RandT g IO) o d)
              (AgentOut o d, Agent (RandT g IO) o d)
runAgent = arrM (\(ain, sf) -> unMSF sf ain)

freezeTime :: AgentOut o d -> AgentOut o d
freezeTime ao = ao { aoFreezeTime = True }

isFreezeTime :: AgentOut o d -> Bool
isFreezeTime = aoFreezeTime

agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId        = aid
  , aiData      = []
  }

agentOut :: AgentOut o d
agentOut = agentOut_ Nothing

agentOutObs :: o -> AgentOut o d
agentOutObs o = agentOut_ (Just o)

agentOut_ :: Maybe o -> AgentOut o d
agentOut_ o = AgentOut {
  aoData        = []
, aoObservable  = o
, aoFreezeTime  = False
}

dataFlow :: DataFlow d -> AgentOut o d -> AgentOut o d
dataFlow df ao = ao { aoData = df : aoData ao }

hasDataFlow :: (d -> Bool) -> AgentIn d -> Bool
hasDataFlow f ai = Data.List.any (f . snd) (aiData ai)

distributeData :: [AgentIn d] -> [(AgentId, AgentOut o d)] -> [AgentIn d]
distributeData ains aouts = map (distributeDataAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllData aouts

    distributeDataAux :: Map.Map AgentId [DataFlow d]
                      -> AgentIn d
                      -> AgentIn d
    distributeDataAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiData ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (\receiverMsgs -> receiverMsgs ++ msgs) mayReceiverMsgs

        ain' = ain { aiData = msgsEvt }

    collectAllData :: [(AgentId, AgentOut o d)] -> Map.Map AgentId [DataFlow d]
    collectAllData aos = foldr collectAllDataAux Map.empty aos
      where
        collectAllDataAux :: (AgentId, AgentOut o d)
                              -> Map.Map AgentId [DataFlow d]
                              -> Map.Map AgentId [DataFlow d]
        collectAllDataAux (senderId, ao) accMsgs 
            | not $ null msgs = foldr collectAllDataAuxAux accMsgs msgs
            | otherwise = accMsgs
          where
            msgs = aoData ao

            collectAllDataAuxAux :: DataFlow d
                                 -> Map.Map AgentId [DataFlow d]
                                 -> Map.Map AgentId [DataFlow d]
            collectAllDataAuxAux (receiverId, m) accMsgs = accMsgs'
              where
                msg = (senderId, m)
                mayReceiverMsgs = Map.lookup receiverId accMsgs
                newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs

                -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
                accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)

printDebugS :: MonadIO m => SF m String ()
printDebugS = arrM (liftIO . putStrLn)