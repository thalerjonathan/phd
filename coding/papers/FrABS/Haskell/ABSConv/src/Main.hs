{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Maybe
import qualified Data.Map as Map
import System.IO
import Debug.Trace

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
  } deriving (Show)

data AgentOut m o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o

  , aoTxBegin     :: Maybe (DataFlow d, AgentTX m o d)
  }

data AgentTXIn d = AgentTXIn
  {
    aiTxData      :: d
  , aiTxCommit    :: Bool
  , aiTxAbort     :: Bool
  } deriving (Show)

-- note that commit returns a agent-sf. this allows
-- to pass state which has changed in the TX to the agent-sf.
-- If we ommit this agent-sf and simply run the existing one 
-- then we couldn't directly pass new state but time-dependent
-- accumulator functions (e.g. time, integral) wouldnt get
-- reset. Maybe we make the function optional using a maybe
-- and if it exists then we use the new one, otherwise run
-- the old one.
-- abort is only a boolean flag and if set to true the
-- old SF and old AgentOut will be restored but with
-- TX begin set to Nothing
data AgentTXOut m o d = AgentTXOut
  {
    aoTxData      :: d
  , aoTxCommit    :: Maybe (Agent m o d, AgentOut m o d)
  , aoTxAbort     :: Bool
  }

type AgentTX m o d        = SF m (AgentTXIn d) (AgentTXOut m o d)

type Agent m o d          = SF m (AgentIn d) (AgentOut m o d)
type AgentObservable o    = (AgentId, Maybe o)

type ConvTestObservable   = Double

data ConvTestData         = Offering Double
                          | OfferingRefuse 
                          | OfferingAccept
                          deriving (Show, Eq)

type ConvTestMonadStack g   = (RandT g IO)

type ConvTestAgentIn        = AgentIn ConvTestData
type ConvTestAgentOut  g    = AgentOut (ConvTestMonadStack g) ConvTestObservable ConvTestData
type ConvTestAgent g        = Agent (ConvTestMonadStack g) ConvTestObservable ConvTestData
type ConvTestAgentTX g      = AgentTX (ConvTestMonadStack g) ConvTestObservable ConvTestData

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

-- FAZIT: 
-- 1. it is f**** cumbersome to implement it using SFs and very hard to get it right
-- 2. it does not make sense as SFs are time-aware but in TXs time halts. 
-- 3. Also the switching would reset a potential time-accumulator (t <- time -< ()). 
-- WHATS THE ALTERNATIVE:
-- use normal functions and follow a callback style as implemented so far
-- the problem is then how to react to incoming TX requests: would need to run
--    the receivers SF with dt=0 with appropriate AgentIn. This would allow the 
--    receiver to change its internal state and update the environment,...
--    but how can the initiator do this, how can it update its state and environment
--    in case of a reply?

-- MAYBE ABANDON THE TX IDEA COMPLETELY AS IT IS TOO DIFFICULT IN FP?
--  maybe it is a completely wrong idea to do in FP?
--  can we emulate it in a different, more functional and reactive way? 
--  the problem is that we will need to employ this kind of switching when we want to follow
--  a protocoll which is very annoying but could be possibly become much
--  easier when providing general solutions to it 
--  an idea would be to halt time and continuously send messages around
--     where agents then transactionally lock their state?

-- TODO: add pro-active mutable environment with STM
-- TODO: test roll-backs of environment as well
-- NOTE: with TX mechanism we can have transactional behaviour with a pro-active environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initTestAgents
  
  obss <- runSimulationUntil g t dt as
  mapM_ (\(t, obs) -> putStrLn ("\nt = " ++ show t) >> mapM_ (putStrLn . show) obs) obss

{--
  let as = map (\aid -> (aid, timeZeroAgent)) [0..agentCount - 1]

  let oM = embed (switchAgentStep0 42) [0, 1]
  o <- runReaderT oM dt
  putStrLn $ show o


switchAgentStep0 :: Int -> SF IO Int Int
switchAgentStep0 i0 = 
    switch
      ((switchAgentStep0Aux i0))
      (\i -> switchAgentStep1 i)
  where
    switchAgentStep0Aux :: Int -> SF IO Int (Int, Event Int)
    switchAgentStep0Aux i0 = proc i -> do
      _ <- arrM (\_ -> liftIO $ putStrLn $ "switchAgentStep0 = " ++ show i0) -< ()
      returnA -< (i0, Event i0)

switchAgentStep1 :: Int -> SF IO Int Int
switchAgentStep1 i0 = 
    switch
      ((switchAgentStep1Aux i0))
      (\i -> switchAgentStep2 i)
  where
    switchAgentStep1Aux :: Int -> SF IO Int (Int, Event Int)
    switchAgentStep1Aux i0 = proc i -> do
      _ <- arrM (\_ -> liftIO $ putStrLn $ "switchAgentStep1 = " ++ show i0) -< ()
      returnA -< (i0, Event i0)

switchAgentStep2 :: Int -> SF IO Int Int
switchAgentStep2 i0 = 
    switch
      ((switchAgentStep2Aux i0) >>> (second notYet))
      (\i -> switchAgentStep0 i)
  where
    switchAgentStep2Aux :: Int -> SF IO Int (Int, Event Int)
    switchAgentStep2Aux i0 = proc i -> do
      _ <- arrM (\_ -> liftIO $ putStrLn $ "switchAgentStep2 = " ++ show i0) -< ()
      returnA -< (i0, Event $ i0 + 1)

timeZeroAgent :: RandomGen g => ConvTestAgent g
timeZeroAgent = proc _ -> do
  t <- time -< ()
  returnA -< agentOutObs t
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
activeAgent w0 env = activeTxAgentInit w0 env

passiveAgent :: RandomGen g 
                 => Double
                 -> ConvTestEnv
                 -> ConvTestAgent g
passiveAgent w0 env = dSwitch checkTxAgent (\bid -> passiveTxAgentAwait w0 env bid)
  where
    checkTxAgent :: RandomGen g
                  => SF (RandT g IO)
                      ConvTestAgentIn 
                      (ConvTestAgentOut, Event Double)
    checkTxAgent = proc ain -> do
      t <- time -< ()
      printDebugS -< ("passiveTx: t = " ++ show t)

      let mayTx = beginTxIn ain
      if isJust mayTx 
        then (do
          let tx = fromJust mayTx
          printDebugS -< ("passiveTX: begin incoming TX = " ++ show tx)
          ret <- passiveTxAgentInit w0 env -< tx
          returnA -< ret)
        else (do
          printDebugS -< ("passiveTX: awaiting incoming TX")
          returnA -< (agentOutObs w0, NoEvent)) -- output does not matter at this point

-------------------------------------------------------------------------------
-- PASSIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
passiveTxAgentInit :: RandomGen g 
                   => Double
                   -> ConvTestEnv
                   -> SF (RandT g IO)
                        (DataFlow ConvTestData) 
                        (ConvTestAgentOut, Event Double)
passiveTxAgentInit w env = proc (_senderId, d) -> 
  if isOffering d 
    then passiveTxAgentReply w -< (offering d)
    else (do
      printDebugS -< ("passiveTX: invalid protocoll, abort TX")
      returnA -< (abortTx agentOut, NoEvent)) -- Invalid protocoll, abort TX

isOffering :: ConvTestData -> Bool
isOffering (Offering _) = True
isOffering _ = False

offering :: ConvTestData -> Double
offering (Offering o) = o
offering _ = error "not an Offering"

passiveTxAgentReply :: RandomGen g 
                    => Double
                    -> SF (RandT g IO)
                        Double 
                        (ConvTestAgentOut, Event Double)
passiveTxAgentReply w = proc v -> do
  printDebugS -< ("passiveTX: received Offering = " ++ show v)

  printDebugS -< ("passiveTX: checking budget constraint, ask = " ++ show v ++ ", wealth = " ++ show w)
  if v >= w
    then (do
      printDebugS -< ("passiveTX: not enough budget, refusing offer")
      let ao = commitTx $ txDataOut OfferingRefuse (agentOutObs w)
      returnA -< (ao, NoEvent))
    else (do
      bid <- arrM_ (getRandomR (0, w)) -< ()
      printDebugS -< ("passiveTX: enough budget, accepting offering, my Offering is " ++ show bid)
      returnA -< (txDataOut (Offering bid) agentOut, Event bid))

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

parSimulation :: (RandomGen g, Show o, Show d)
              => [Agent (RandT g IO) o d] 
              -> [AgentIn d] 
              -> SF (RandT g IO)
                  ()
                  (Time, [(AgentId, AgentOut m o d)])
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: (RandomGen g, Show o, Show d)
                     => SF (RandT g IO)
                          ((), ([Agent (RandT g IO) o d], [AgentIn d]))
                          ((Time, [(AgentId, AgentOut m o d)]), ([Agent (RandT g IO) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      (msfs, aos) <- runAgents -< (msfs, ains)
      t           <- time      -< ()

      let aios = map (\(ao, ai) -> (agentId ai, ao)) (zip aos ains)

      (aios', msfs') <- runTransactions -< (aios, msfs)

      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeData ains' aios'

      returnA -< ((t, aios'), (msfs', ains''))

    -- TX need to be executed sequentially...
    runTransactions :: (Show o, Show d)
                    => SF (RandT g IO) 
                          ([(AgentId, AgentOut m o d)], [Agent (RandT g IO) o d])
                          ([(AgentId, AgentOut m o d)], [Agent (RandT g IO) o d])
    runTransactions = proc (aios, sfs) -> do
        let els = zip aios sfs
        -- printDebugS -< ("els = " ++ show aios)
        let m = foldr (\((aid, ao), sf) m' -> Map.insert aid (ao, sf) m') Map.empty els
        m' <- runTransactionsAux -< (els, m)
        let ml = Map.toList m'
        let aiosMsfs@(aios, _) = foldr (\(aid, (ao, sf)) (accAio, accSf) -> ((aid, ao) : accAio, sf : accSf)) ([], []) ml
        printDebugS -< ("aios = " ++ show aios)
        returnA -< aiosMsfs

      where
        runTransactionsAux :: (Show o, Show d)
                           => SF (RandT g IO)
                                (([((AgentId, AgentOut m o d), Agent (RandT g IO) o d)]),
                                  (Map.Map AgentId (AgentOut m o d, Agent (RandT g IO) o d)))
                                (Map.Map AgentId (AgentOut m o d, Agent (RandT g IO) o d))
        runTransactionsAux = proc (els, m) -> do
          if null els
            then returnA -< m
            else (do 
              let e@((aid, ao), sf) = head els
              if (isJust $ aoTxBegin ao)
                then (do
                  printDebugS -< ("found TX pair, running TX...")
                  m' <- runTxPair -< (e, m)
                  runTransactionsAux -< (tail els, m'))
                else runTransactionsAux -< (tail els, m))

        -- care must be taken if two agents want to start a TX with each other at the same time
        -- note that we allow agents to transact with themselves
        runTxPair :: (Show o, Show d)
                  => SF (RandT g IO)
                      (((AgentId, AgentOut m o d), Agent (RandT g IO) o d),
                        (Map.Map AgentId (AgentOut m o d, Agent (RandT g IO) o d)))
                      (Map.Map AgentId (AgentOut m o d, Agent (RandT g IO) o d))
        runTxPair = proc (((aid, ao), sf), m) -> do
          let (rAid, _) = fromJust $ aoTxBegin ao
          let (rAo, rSf) = fromJust $ Map.lookup rAid m -- TODO: proper handling of Maybe

          mayTx <- runTxBegin -< ((aid, ao, sf), (rAid, rAo, rSf))
          if isNothing mayTx
            then (do
              printDebugS -< ("transaction aborted")
              returnA -< m)
            else (do
              printDebugS -< ("transaction finished, committing...")
              let ((aid1, ao1, sf1), (aid2, ao2, sf2)) = fromJust mayTx

              printDebugS -< ("aid1 = " ++ show aid1 ++ ", ao1 = " ++ show ao1)
              printDebugS -< ("aid2 = " ++ show aid2 ++ ", ao2 = " ++ show ao2)
              
              -- TODO: reset the transaction related fields?

              let m' = Map.insert aid1 (ao1, sf1) m
              let m'' = Map.insert aid2 (ao2, sf2) m'
              
              -- TODO: commit environment changes as well when we had a STM environment
              printDebugS -< ("transaction committed")

              returnA -< m'')

    runTxBegin :: (Show o, Show d)
               => SF (RandT g IO) 
                    ((AgentId, AgentOut m o d, Agent (RandT g IO) o d), 
                     (AgentId, AgentOut m o d, Agent (RandT g IO) o d))
                    (Maybe 
                      ((AgentId, AgentOut m o d, Agent (RandT g IO) o d), 
                       (AgentId, AgentOut m o d, Agent (RandT g IO) o d)))
    runTxBegin = proc ((sAid, sAo0, sSf0), (rAid, rAo0, rSf0)) -> do
      let rAin = (agentIn rAid) { aiTxBegin = Just (sAid, snd $ fromJust $ aoTxBegin sAo0) }
      (rAo', rSf') <- runAgentWithDt 0 -< (rAin, rSf0)

      runTx -< ((rAid, rAo', rSf'), (sAid, sAo0, sSf0))

    runTx :: (Show o, Show d)
          => SF (RandT g IO)
              ((AgentId, AgentOut m o d, Agent (RandT g IO) o d), 
               (AgentId, AgentOut m o d, Agent (RandT g IO) o d))
              (Maybe
                ((AgentId, AgentOut m o d, Agent (RandT g IO) o d), 
                 (AgentId, AgentOut m o d, Agent (RandT g IO) o d)))
    runTx = proc ((sAid, sAo, sSf), (rAid, rAo, rSf)) -> do
      printDebugS -< ("runTx: sAo = " ++ show sAo)
      printDebugS -< ("runTx: rAo = " ++ show rAo)
      
      -- we terminate the running transaction if
      --    the sending agent wants to abort it
      --    the sending agent didn't send a tx-data
      if aoTxAbort sAo 
        then returnA -< Nothing
        else (do
          if aoTxCommit rAo && aoTxCommit sAo
            then returnA -< Just ((sAid, sAo, sSf), (rAid, rAo, rSf))
            else (do
              let rAin = (agentIn rAid) { aiTxData = aoTxData sAo }
              (rAo', rSf') <- runAgentWithDt 0 -< (rAin, rSf)

              -- either both commit at this point, or we
              -- recursively run the tx in a next step
              if aoTxCommit rAo' && aoTxCommit sAo
                then returnA -< Just ((sAid, sAo, sSf), (rAid, rAo', rSf'))
                else runTx -< ((rAid, rAo', rSf'), (sAid, sAo, sSf))))

runAgents :: RandomGen g
          => SF (RandT g IO)
              ([Agent (RandT g IO) o d], [AgentIn d]) 
              ([Agent (RandT g IO) o d], [AgentOut m o d])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns = zipWith (\sf ain -> (dt, (ain, sf))) sfs ins
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)
  where
    runAgent :: RandomGen g
              => SF (RandT g IO)
                  (AgentIn d, Agent (RandT g IO) o d)
                  (AgentOut m o d, Agent (RandT g IO) o d)
    runAgent = arrM (\(ain, sf) -> unMSF sf ain)

runAgentWithDt :: Double
              -> SF (RandT g IO)
                    (AgentIn d, Agent (RandT g IO) o d)
                    (AgentOut m o d, Agent (RandT g IO) o d)
runAgentWithDt dt = readerS $ proc (_, (ain, sf)) -> do
    (ao, sf') <- runReaderS_ runAgent dt -< (ain, sf)
    returnA -< (ao, sf')

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

beginTx :: DataFlow d -> AgentOut m o d -> AgentOut m o d
beginTx df ao = ao { aoTxBegin = Just df }

txDataOut :: d -> AgentOut m o d -> AgentOut m o d
txDataOut d ao = ao { aoTxData = Just d }

commitTx :: AgentOut m o d -> AgentOut m o d
commitTx ao = ao { aoTxCommit = True }

abortTx :: AgentOut m o d -> AgentOut m o d
abortTx ao = ao { aoTxAbort = True}

agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut m o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId        = aid
  , aiData      = []

  , aiTxBegin   = Nothing
  }

agentOut :: AgentOut m o d
agentOut = agentOut_ Nothing

agentOutObs :: o -> AgentOut m o d
agentOutObs o = agentOut_ (Just o)

agentOut_ :: Maybe o -> AgentOut m o d
agentOut_ o = AgentOut {
  aoData        = []
, aoObservable  = o

, aoTxBegin     = Nothing
}

dataFlow :: DataFlow d -> AgentOut m o d -> AgentOut m o d
dataFlow df ao = ao { aoData = df : aoData ao }

distributeData :: [AgentIn d] -> [(AgentId, AgentOut m o d)] -> [AgentIn d]
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

    collectAllData :: [(AgentId, AgentOut m o d)] -> Map.Map AgentId [DataFlow d]
    collectAllData aos = foldr collectAllDataAux Map.empty aos
      where
        collectAllDataAux :: (AgentId, AgentOut m o d)
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