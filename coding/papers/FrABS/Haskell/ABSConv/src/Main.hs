{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Maybe
import qualified Data.Map as Map
import System.IO

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

  , aiTxBegin   :: Maybe (DataFlow d)
  , aiTxData    :: Maybe d
  , aiTxCommit  :: Bool
  , aiTxAbort   :: Bool
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o

  , aoTxBegin     :: Maybe (DataFlow d)
  , aoTxData      :: Maybe d
  , aoTxCommit    :: Bool
  , aoTxAbort     :: Bool
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
type ConvTestAgent g        = Agent (Rand g) ConvTestObservable ConvTestData

type ConvTestEnv            = [AgentId]

agentCount :: Int
agentCount = 2

rngSeed :: Int
rngSeed = 42

t :: Time
t = 10

dt :: DTime
dt = 1.0

initAgentWealth :: Double
initAgentWealth = 100

-- TODO: add pro-active mutable environment with STM
-- NOTE: with TX mechanism we can have transactional behaviour with a pro-active environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initTestAgents
  --let as = map (\aid -> (aid, timeZeroAgent)) [0..agentCount - 1]

  obss <- runSimulationUntil g t dt as

  mapM_ (\(t, obs) -> putStrLn ("\nt = " ++ show t) >> mapM_ (putStrLn . show) obs) obss

timeZeroAgent :: RandomGen g => ConvTestAgent g
timeZeroAgent = proc _ -> do
  t <- time -< ()
  returnA -< agentOutObs t

initTestAgents :: RandomGen g 
               => [(AgentId, ConvTestAgent g)]
initTestAgents = [aa, pa] 
  where
    aa = (0, activeAgent 100 [1])
    pa = (1, passiveAgent 100 [0])

activeAgent :: RandomGen g 
                => Double
                -> ConvTestEnv
                -> ConvTestAgent g
activeAgent w0 env = activeTxAgentInit w0 env

passiveAgent :: RandomGen g 
                 => Double
                 -> ConvTestEnv
                 -> ConvTestAgent g
passiveAgent w0 env = switch checkTxAgent txCont
  where
    checkTxAgent :: RandomGen g
                  => SF (Rand g) 
                      ConvTestAgentIn 
                      (ConvTestAgentOut, Event (Maybe (DataFlow ConvTestData)))
    checkTxAgent = arr (\ain -> (agentOutObs w0, Event (beginTxIn ain)))

    txCont :: RandomGen g
           => Maybe (DataFlow ConvTestData) 
           -> ConvTestAgent g
    txCont (Just df)  = passiveTxAgentInit df w0 env  -- react to incoming TX
    txCont Nothing    = passiveAgent w0 env

-------------------------------------------------------------------------------
-- PASSIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
passiveTxAgentInit :: RandomGen g 
                   => DataFlow ConvTestData
                   -> Double
                   -> ConvTestEnv
                   -> ConvTestAgent g
passiveTxAgentInit (_senderId, Offering v) w env = 
  become -- TODO: we need to wait for the reply which means we cannot switch immediately but delay by 1 step 
    (passiveTxAgentReply v w)
    (passiveTxAgentAwait w env)
passiveTxAgentInit (_, _) _ _ = 
    arr (\_ -> abortTx agentOut) -- Invalid protocoll, abort TX

passiveTxAgentReply :: RandomGen g 
                    => Double
                    -> Double
                    -> SF (Rand g) ConvTestAgentIn (ConvTestAgentOut, Maybe Double)
passiveTxAgentReply v w = proc _ -> do
  rw <- arrM_ (getRandomR (0, w)) -< ()
  if v >= w
    then returnA -< (txDataOut OfferingRefuse agentOut, Nothing)
    else returnA -< (txDataOut (Offering rw) agentOut, Just rw)

passiveTxAgentAwait :: RandomGen g
                    => Double
                    -> ConvTestEnv
                    -> Maybe Double
                    -> ConvTestAgent g
passiveTxAgentAwait w env Nothing =  
  -- in this case nothing changes as the passive agent has not enough wealth, but still commit TX
  switch 
    (arr (\_ -> (commitTx agentOut, Event ())))
    (\_ -> passiveAgent w env)
passiveTxAgentAwait w env (Just rw) = 
    switch  -- TODO: delay switch
      checkPassiveTxAgentAwait
      (\w' -> passiveAgent w' env)
  where
    checkPassiveTxAgentAwait :: RandomGen g
                             => SF (Rand g) 
                                  ConvTestAgentIn 
                                  (ConvTestAgentOut, Event Double)
    checkPassiveTxAgentAwait = proc ain -> do
      if hasTxData ain 
        then returnA -< handleReply (txDataIn ain) w rw
        else returnA -< (abortTx agentOut, NoEvent) -- abort because expected reply by active agent

    handleReply :: ConvTestData
                -> Double
                -> Double
                -> (ConvTestAgentOut, Event Double)
    -- active agent refuses, no exchange but commit TX
    handleReply OfferingRefuse w _  = (commitTx agentOut, Event w) 
    -- active agent accepts, make exchange and commit TX
    handleReply OfferingAccept w rw = (commitTx agentOut, Event $ w - rw) 
    -- abort because wrong protocoll 
    handleReply _              _  _ = (abortTx agentOut, NoEvent) 


-------------------------------------------------------------------------------
-- ACTIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
activeTxAgentInit :: RandomGen g 
                  => Double
                  -> ConvTestEnv
                  -> ConvTestAgent g
activeTxAgentInit w env = 
  become
    (activeTxAgentBegin w env)
    (activeTxAgentAwait w env)

activeTxAgentBegin :: RandomGen g 
                   => Double
                   -> ConvTestEnv
                   -> SF (Rand g) ConvTestAgentIn (ConvTestAgentOut, Double)
activeTxAgentBegin w env = proc ain -> do
  let aid = agentId ain
  raid <- arrM_ (getRandomR (0, length env - 1)) -< ()
  if aid == raid
    then activeTxAgentBegin w env -< ain
    else (do
      rask <- arrM_ (getRandomR (0, w)) -< ()
      returnA -< (beginTx (raid, Offering rask) agentOut, rask))

activeTxAgentAwait :: RandomGen g
                   => Double
                   -> ConvTestEnv
                   -> Double
                   -> ConvTestAgent g
activeTxAgentAwait w env rask =  
    dSwitch  -- TODO: delay switch
      checkActiveTxAgentAwait
      (\w' -> activeAgent w' env)
  where
    checkActiveTxAgentAwait :: RandomGen g
                            => SF (Rand g) 
                                  ConvTestAgentIn 
                                  (ConvTestAgentOut, Event Double)
    checkActiveTxAgentAwait = proc ain -> do
      if hasTxData ain 
        then returnA -< handleReply (txDataIn ain) w rask
        else returnA -< (abortTx agentOut, NoEvent) -- abort because expected reply by passive agent

    handleReply :: ConvTestData
                -> Double
                -> Double
                -> (ConvTestAgentOut, Event Double)
    -- passive agent refuses, no exchange but commit TX
    handleReply OfferingRefuse w _    = (commitTx agentOut, Event w) 
    -- passive agent accepts, make exchange and commit TX
    handleReply OfferingAccept w rask = (commitTx agentOut, Event $ w + rask)
    -- abort because wrong protocoll 
    handleReply _              _  _   = (abortTx agentOut, NoEvent) 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
runSimulationUntil :: RandomGen g
                   => g
                   -> Time 
                   -> DTime
                   -> [(AgentId, Agent (Rand g) o d)]
                   -> IO [(Time, [AgentObservable o])]
runSimulationUntil g t dt aiMsfs = do
  let steps = floor $ t / dt
  let ticks = replicate steps ()

  let ais = map fst aiMsfs
  let msfs = map snd aiMsfs
  let ains  = map agentIn ais

  let aossM = embed (parSimulation msfs ains) ticks

  let readerM = runReaderT aossM dt
  let aoss = evalRand readerM g

  let aobs = map (\(t, aos) -> (t, map (\(aid, ao) -> (aid, agentObservable ao)) aos)) aoss

  return aobs

-- TODO: implement TXs
--       we need to keep track of the SF before the start of the TX, if the TX is
--       aborted, we will switch back to it. If the TX is commited, then we will
--       switch into the TX continuation
--       RUN WITH DT = 0!
--       on abort we need to rollback the environment STM which means we are simply NOT calling atomically
        -- a commited TX results in updated agentout and SF of both agents
        -- an aborted TX leaves the agentouts and SFs of both agents as before 
        --    TX started but resets all tx-related flags
        --       this is not correct yet, we need to clearly think about what it means
        --       to commit a TX!

parSimulation :: RandomGen g
              => [Agent (Rand g) o d] 
              -> [AgentIn d] 
              -> SF (Rand g)
                  ()
                  (Time, [(AgentId, AgentOut o d)])
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: RandomGen g
                     => SF (Rand g)
                          ((), ([Agent (Rand g) o d], [AgentIn d]))
                          ((Time, [(AgentId, AgentOut o d)]), ([Agent (Rand g) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      (msfs, aos) <- runAgents -< (msfs, ains)
      t           <- time      -< ()

      let aios = map (\(ao, ai) -> (agentId ai, ao)) (zip aos ains)

      (aios', msfs') <- runTransactions -< (aios, msfs)

      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeData ains' aios'

      returnA -< ((t, aios'), (msfs, ains''))

    -- TX need to be executed sequentially...
    runTransactions :: SF (Rand g) 
                          ([(AgentId, AgentOut o d)], [Agent (Rand g) o d])
                          ([(AgentId, AgentOut o d)], [Agent (Rand g) o d])
    runTransactions = proc (aios, sfs) -> do
        let els = zip aios sfs
        let m = foldr (\((aid, ao), sf) m' -> Map.insert aid (ao, sf) m') Map.empty els
        m' <- runTransactionsAux -< (els, m)
        let ml = Map.toList m'
        let aiosMsfs = foldr (\(aid, (ao, sf)) (accAio, accSf) -> ((aid, ao) : accAio, sf : accSf)) ([], []) ml
        returnA -< aiosMsfs

      where
        runTransactionsAux :: SF (Rand g)
                                (([((AgentId, AgentOut o d), Agent (Rand g) o d)]),
                                  (Map.Map AgentId (AgentOut o d, Agent (Rand g) o d)))
                                (Map.Map AgentId (AgentOut o d, Agent (Rand g) o d))
        runTransactionsAux = proc (els, m) -> do
          if null els
            then returnA -< m
            else (do 
              let e@((aid, ao), sf) = head els
              if (isJust $ aoTxBegin ao)
                then (do
                  m' <- runTxPair -< (e, m)
                  runTransactionsAux -< (tail els, m'))
                else runTransactionsAux -< (tail els, m))

        -- care must be taken if two agents want to start a TX with each other at the same time
        -- note that we allow agents to transact with themselves
        runTxPair :: SF (Rand g)
                      (((AgentId, AgentOut o d), Agent (Rand g) o d),
                        (Map.Map AgentId (AgentOut o d, Agent (Rand g) o d)))
                      (Map.Map AgentId (AgentOut o d, Agent (Rand g) o d))
        runTxPair = proc (((aid, ao), sf), m) -> do
          let (rAid, _) = fromJust $ aoTxBegin ao
          let (rAo, rSf) = fromJust $ Map.lookup rAid m -- TODO: proper handling of Maybe

          mayTx <- runTxBegin -< ((aid, ao, sf), (rAid, rAo, rSf))
          if isNothing mayTx
            then returnA -< m
            else (do
              let ((aid1, ao1, sf1), (aid2, ao2, sf2)) = fromJust mayTx

              let m' = Map.insert aid1 (ao1, sf1) m
              let m'' = Map.insert aid2 (ao2, sf2) m
              
              -- TODO: commit environment changes as well when we had a STM environment

              returnA -< m')

    runTxBegin :: SF (Rand g) 
                    ((AgentId, AgentOut o d, Agent (Rand g) o d), 
                     (AgentId, AgentOut o d, Agent (Rand g) o d))
                    (Maybe 
                      ((AgentId, AgentOut o d, Agent (Rand g) o d), 
                       (AgentId, AgentOut o d, Agent (Rand g) o d)))
    runTxBegin = proc ((sAid, sAo0, sSf0), (rAid, rAo0, rSf0)) -> do
      let rAin = (agentIn rAid) { aiTxBegin = Just (sAid, snd $ fromJust $ aoTxBegin sAo0) }
      (rAo', rSf') <- runAgentWithDt 0 -< (rAin, rSf0)

      runTx -< ((rAid, rAo', rSf'), (sAid, sAo0, sSf0))

    runTx :: SF (Rand g)
              ((AgentId, AgentOut o d, Agent (Rand g) o d), 
               (AgentId, AgentOut o d, Agent (Rand g) o d))
              (Maybe
                ((AgentId, AgentOut o d, Agent (Rand g) o d), 
                 (AgentId, AgentOut o d, Agent (Rand g) o d)))
    runTx = proc ((sAid, sAo, sSf), (rAid, rAo, rSf)) -> do
      -- we terminate the running transaction if
      --    the sending agent wants to abort it
      --    the sending agent didn't send a tx-data
      if aoTxAbort sAo || (isNothing $ aoTxData sAo)
        then returnA -< Nothing
        else (do
          let rAin = (agentIn rAid) { aiTxData = aoTxData sAo }
          (rAo', rSf') <- runAgentWithDt 0 -< (rAin, rSf)

          -- either both commit at this point, or we
          -- recursively run the tx in a next step
          if aoTxCommit rAo && aoTxCommit sAo
            then returnA -< Just ((sAid, sAo, sSf), (rAid, rAo', rSf'))
            else runTx -< ((rAid, rAo', rSf'), (sAid, sAo, sSf)))

runAgents :: RandomGen g
          => SF (Rand g)
              ([Agent (Rand g) o d], [AgentIn d]) 
              ([Agent (Rand g) o d], [AgentOut o d])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns = zipWith (\sf ain -> (dt, (ain, sf))) sfs ins
    --arets <- mapMSF (runReaderS runAgent) -< asIns
    arets <- mapMSF (runReaderS (runAgentWithDt dt)) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)
  where
    runAgent :: RandomGen g
              => SF (Rand g)
                  (AgentIn d, Agent (Rand g) o d)
                  (AgentOut o d, Agent (Rand g) o d)
    runAgent = arrM (\(ain, sf) -> unMSF sf ain)

runAgentWithDt :: Double
              -> SF (Rand g)
                    (AgentIn d, Agent (Rand g) o d)
                    (AgentOut o d, Agent (Rand g) o d)
runAgentWithDt dt = readerS $ proc (dt, (ain, sf)) -> do
    (ao, sf') <- runReaderS_ runAgentWithDtAux dt -< (ain, sf)
    returnA -< (ao, sf')
  where
    runAgentWithDtAux :: SF (Rand g)
                          (AgentIn d, Agent (Rand g) o d)
                          (AgentOut o d, Agent (Rand g) o d)
    runAgentWithDtAux = arrM (\(ain, sf) -> unMSF sf (ain))

isBeginTx :: AgentIn d -> Bool
isBeginTx = isJust . aiTxBegin

beginTxData :: AgentIn d -> DataFlow d
beginTxData = fromJust . aiTxBegin

beginTxIn :: AgentIn d -> Maybe (DataFlow d)
beginTxIn = aiTxBegin

hasTxData :: AgentIn d -> Bool
hasTxData = isJust . aiTxData

txDataIn :: AgentIn d -> d
txDataIn = fromJust . aiTxData

isCommitTx :: AgentIn d -> Bool
isCommitTx = aiTxCommit

isAbortTx :: AgentIn d -> Bool
isAbortTx = aiTxAbort

beginTx :: DataFlow d -> AgentOut o d -> AgentOut o d
beginTx df ao = ao { aoTxBegin = Just df }

txDataOut :: d -> AgentOut o d -> AgentOut o d
txDataOut d ao = ao { aoTxData = Just d }

commitTx :: AgentOut o d -> AgentOut o d
commitTx ao = ao { aoTxCommit = True }

abortTx :: AgentOut o d -> AgentOut o d
abortTx ao = ao { aoTxAbort = True}

agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId        = aid
  , aiData      = []

  , aiTxBegin   = Nothing
  , aiTxData    = Nothing
  , aiTxCommit  = False
  , aiTxAbort   = False
  }

agentOut :: AgentOut o d
agentOut = agentOut_ Nothing

agentOutObs :: o -> AgentOut o d
agentOutObs o = agentOut_ (Just o)

agentOut_ :: Maybe o -> AgentOut o d
agentOut_ o = AgentOut {
  aoData        = []
, aoObservable  = o

, aoTxBegin     = Nothing
, aoTxData      = Nothing
, aoTxCommit    = False
, aoTxAbort     = False
}

dataFlow :: DataFlow d -> AgentOut o d -> AgentOut o d
dataFlow df ao = ao { aoData = df : aoData ao }

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

become :: Monad m => SF m a (b, c) -> (c -> SF m a b) -> SF m a b
become sfFirst sfSecond = switch (becomeAux sfFirst) (\c -> sfSecond c)
  where
    becomeAux :: Monad m 
              => SF m a (b, c) 
              -> SF m a (b, Event c)
    becomeAux sfFirst = proc a -> do
      (b, c) <- sfFirst -< a
      returnA -< (b, Event c)