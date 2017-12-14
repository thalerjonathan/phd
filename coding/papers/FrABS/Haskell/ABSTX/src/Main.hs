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

  , aiRequestTx :: Maybe (DataFlow d)
  } deriving (Show)

data AgentOut m o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o

  , aoRequestTx   :: Maybe (DataFlow d, AgentTX m o d)
  , aoAcceptTx    :: Maybe (d, AgentTX m o d)
  }

data AgentTXIn d = AgentTXIn
  {
    aiTxData      :: Maybe d
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
    aoTxData      :: Maybe d
  , aoTxCommit    :: Maybe (AgentOut m o d, Maybe (Agent m o d))
  , aoTxAbort     :: Bool
  }

type AgentTX m o d        = SF m (AgentTXIn d) (AgentTXOut m o d)

type Agent m o d          = SF m (AgentIn d) (AgentOut m o d)
type AgentObservable o    = (AgentId, Maybe o)

type TXTestObservable     = Double

data TXTestProtocoll      = Offering Double
                          | OfferingRefuse 
                          | OfferingAccept
                          deriving (Show, Eq)

type TXTestMonadStack g   = (RandT g IO)

type TXTestAgentIn        = AgentIn TXTestProtocoll
type TXTestAgentTXIn      = AgentTXIn TXTestProtocoll

type TXTestAgentOut g     = AgentOut (TXTestMonadStack g) TXTestObservable TXTestProtocoll
type TXTestAgentTXOut g   = AgentTXOut (TXTestMonadStack g) TXTestObservable TXTestProtocoll

type TXTestAgent g        = Agent (TXTestMonadStack g) TXTestObservable TXTestProtocoll
type TXTestAgentTX g      = AgentTX (TXTestMonadStack g) TXTestObservable TXTestProtocoll

type TXTestEnv            = [AgentId]

agentCount :: Int
agentCount = 2

rngSeed :: Int
rngSeed = 42

t :: Time
t = 10

timeDelta :: DTime
timeDelta = 1.0

initAgentCash :: Double
initAgentCash = 100

-- TODO: add pro-active mutable environment with STM
-- TODO: test roll-backs of environment as well
-- NOTE: with TX mechanism we can have transactional behaviour with a pro-active environment

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed
  let as = initTestAgents
  
  obss <- runSimulationUntil g t timeDelta as
  mapM_ (\(t, obs) -> putStrLn ("\nt = " ++ show t) >> mapM_ (putStrLn . show) obs) obss

initTestAgents :: RandomGen g 
               => [(AgentId, TXTestAgent g)]
initTestAgents = [aa, pa] 
  where
    aa = (0, activeAgent initAgentCash [1])
    pa = (1, passiveAgent initAgentCash [0])

-------------------------------------------------------------------------------
-- ACTIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
activeAgent :: RandomGen g 
                => Double
                -> TXTestEnv
                -> TXTestAgent g
activeAgent cash env = activeTxAgentBegin cash env

activeTxAgentBegin :: RandomGen g 
                   => Double
                   -> TXTestEnv
                   -> TXTestAgent g
activeTxAgentBegin cash env = proc ain -> do
  let aid = agentId ain
  ridx <- arrM_ (getRandomR (0, length env - 1)) -< ()
  let raid = env !! ridx
  
  t <- time -< ()
  printDebugS -< ("activeTX: t = " ++ show t)
  printDebugS -< ("activeTX: drawing random agentid = " ++ show raid)
  
  if aid == raid
    then activeTxAgentBegin cash env -< ain
    else (do
      ask <- arrM_ (getRandomR (0, cash)) -< ()
      let d = (raid, Offering ask)
      printDebugS -< ("activeTX: requestTx = " ++ show d)
      returnA -< (requestTx d (activeTxAgentTx ask cash env) agentOut))

activeTxAgentTx :: RandomGen g
                => Double
                -> Double
                -> TXTestEnv
                -> TXTestAgentTX g
activeTxAgentTx ask cash env = proc txIn -> do
    let txData = txDataIn txIn
    printDebugS -< ("activeTX: received tx reply = " ++ show txData)
    returnA -< handleReply txData ask cash env

  where
    handleReply :: RandomGen g
                => TXTestProtocoll
                -> Double
                -> Double
                -> TXTestEnv
                -> TXTestAgentTXOut g
    -- passive agent refuses, no exchange but commit TX
    handleReply OfferingRefuse _ cash _  = trace ("activeTX: passive agent refuses, commit") 
      (commitTx 
        (agentOutObs cash) 
        agentTXOut)

    handleReply (Offering bid) ask cash env
        | bid >= ask = trace ("activeTX: no crossover, commit") 
          (commitTxWithCont 
            (agentOutObs cash')
            (activeAgent cash' env)
            (txDataOut OfferingAccept agentTXOut))

        | otherwise = trace ("activeTX: crossover, commit")
          (commitTx 
            (agentOutObs cash)
            (txDataOut OfferingRefuse agentTXOut))
      where
        cash' = cash + ask

    -- protocoll error
    handleReply _ _ _ _ = trace ("activeTX: protocoll error, abort") 
      abortTx agentTXOut 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- PASSIVE TX BEHAVIOUR
-------------------------------------------------------------------------------
passiveAgent :: RandomGen g 
                 => Double
                 -> TXTestEnv
                 -> TXTestAgent g
passiveAgent cash env = proc ain -> do
    t <- time -< ()
    printDebugS -< ("passiveTx: t = " ++ show t)

    if isRequestTx ain 
      then (do
        let txReq@(senderId, d) = requestTxData ain

        printDebugS -< ("passiveTX: has tx request = " ++ show txReq)

        -- TODO: randomly turn down the transaction by returning Nothing in beginTx

        if isOffering d 
          then handleRequest cash env -< (offering d, ain)
          else returnA -< agentOut) -- Invalid protocoll, refuse TX by setting Nothing in aoAcceptTx

      else (do
        printDebugS -< ("passiveTX: awaiting TX request")
        returnA -< (agentOutObs cash))

  where
    isOffering :: TXTestProtocoll -> Bool
    isOffering (Offering _) = True
    isOffering _            = False

    offering :: TXTestProtocoll -> Double
    offering (Offering v) = v
    offering _            = error "No Offering!"

    handleRequest :: RandomGen g 
                  => Double
                  -> TXTestEnv
                  -> SF (TXTestMonadStack g) 
                      (Double, TXTestAgentIn) 
                      (TXTestAgentOut g)
    handleRequest cash env = proc (ask, _) -> do
      printDebugS -< ("passiveTX: received Offering with ask = " ++ show ask)
      
      printDebugS -< ("passiveTX: checking budget constraint, ask = " ++ show ask ++ ", cash = " ++ show cash)
      if ask > cash
        then (do
          printDebugS -< ("passiveTX: not enough cash, accepting TX but send OfferRefuse reply")
          returnA -< acceptTX 
                      (OfferingRefuse)
                      (passiveTxAgentTx cash ask 0 env) -- no bid => set to 0
                      (agentOutObs cash))
        else (do
          bid <- arrM_ (getRandomR (0, cash)) -< ()
          printDebugS -< ("passiveTX: enough budget, accepting TX and accepting offering, my Offering is " ++ show bid)
          returnA -< acceptTX 
                      (Offering bid)
                      (passiveTxAgentTx cash ask bid env) 
                      (agentOutObs cash))

passiveTxAgentTx :: RandomGen g
                 => Double
                 -> Double
                 -> Double
                 -> TXTestEnv
                 -> TXTestAgentTX g
passiveTxAgentTx cash ask bid env = proc txIn -> do
    printDebugS -< ("passiveTxAgentTx: received reply " ++ show txIn)

    if hasTxDataIn txIn 
      then returnA -< handleReply (txDataIn txIn) cash ask bid env
      else (do
        -- we receive an empty data reply with a commit as an ACK from the active agent
        -- who ACKs that it has received our previously OfferingRefuse which was due
        -- to not enough cash => if no commit is there then something is wrong,
        -- if its there, we commit the TX on this side
        if isCommitTX txIn 
          then returnA -< commitTx (agentOutObs cash) agentTXOut
          else returnA -< abortTx agentTXOut)

  where
    handleReply :: RandomGen g
                => TXTestProtocoll
                -> Double
                -> Double
                -> Double
                -> TXTestEnv
                -> TXTestAgentTXOut g
    -- active agent refuses, no exchange but commit TX
    handleReply OfferingRefuse cash _ _ _ = trace ("passiveTX: OfferingRefuse, commit")
      (commitTx 
        (agentOutObs cash)
        agentTXOut)

    -- active agent accepts, make exchange and commit TX
    handleReply OfferingAccept cash ask bid env = trace ("passiveTX: OfferingAccept, commit")
      (commitTxWithCont 
        (agentOutObs cash')
        (activeAgent cash' env)
        (txDataOut OfferingAccept agentTXOut))
      where
        cash' = cash - bid

    -- abort because wrong protocoll 
    handleReply _ _ _ _ _ = trace ("passiveTX: protocoll fault, abort") (abortTx agentTXOut)
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
                  (Time, [(AgentId, AgentOut (RandT g IO) o d)])
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: (RandomGen g, Show o, Show d)
                     => SF (RandT g IO)
                          ((), ([Agent (RandT g IO) o d], [AgentIn d]))
                          ((Time, [(AgentId, AgentOut (RandT g IO) o d)]), ([Agent (RandT g IO) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      (msfs, aos) <- runAgents -< (msfs, ains)
      t           <- time      -< ()

      let aios = map (\(ao, ai) -> (agentId ai, ao)) (zip aos ains)
      let aios' = aios
      let msfs' = msfs

      (aios', msfs') <- runTransactions -< (aios, msfs)

      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeData ains' aios'

      returnA -< ((t, aios'), (msfs', ains''))

    -- TX need to be executed sequentially...
    runTransactions :: (Show o, Show d)
                    => SF (RandT g IO) 
                          ([(AgentId, AgentOut (RandT g IO) o d)], [Agent (RandT g IO) o d])
                          ([(AgentId, AgentOut (RandT g IO) o d)], [Agent (RandT g IO) o d])
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
                                  (Map.Map AgentId (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d)))
                                (Map.Map AgentId (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d))
        runTransactionsAux = proc (els, m) -> do
          if null els
            then returnA -< m
            else (do 
              let e@((aid, ao), sf) = head els
              if (isJust $ aoRequestTx ao)
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
                        (Map.Map AgentId (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d)))
                      (Map.Map AgentId (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d))
        runTxPair = proc (((sAid, sAo0), sSf0), m) -> do
          let ((rAid, d), sTxSf) = fromJust $ aoRequestTx sAo0
          let (rAo0, rSf0) = fromJust $ Map.lookup rAid m -- TODO: proper handling of Maybe
          
          let rAin = (agentIn rAid) { aiRequestTx = Just (sAid, d) }

          -- ignoring the sf of this run makes it as it has never happened,
          -- but what if the agent changes the environment??
          (rAo', _) <- runAgentWithDt 0 -< (rAin, rSf0) 

          -- TODO: handle aiAcceptTx
          let mayAcceptTx = aoAcceptTx rAo'

          -- TODO proper handling of Maybe, it its nothing, then tell the requesting agent that it aborted
          let (d, rTxSf) = fromJust mayAcceptTx

          mayTx <- runTx -< ((aid, ao, sf), (rAid, rAo, rSf))
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

    runTx :: (Show o, Show d)
          => SF (RandT g IO)
              ((AgentId, AgentOut (RandT g IO) o d, Agent (RandT g IO) o d), 
               (AgentId, AgentOut (RandT g IO) o d, Agent (RandT g IO) o d))
              (Maybe
                ((AgentId, AgentOut (RandT g IO) o d, Agent (RandT g IO) o d), 
                 (AgentId, AgentOut (RandT g IO) o d, Agent (RandT g IO) o d)))
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
              ([Agent (RandT g IO) o d], [AgentOut (RandT g IO) o d])
runAgents = readerS $ proc (dt, (sfs, ins)) -> do
    let asIns = zipWith (\sf ain -> (dt, (ain, sf))) sfs ins
    arets <- mapMSF (runReaderS runAgent) -< asIns
    let (aos, sfs') = unzip arets
    returnA -< (sfs', aos)

runAgentWithDt :: RandomGen g
               => Double
               -> SF (RandT g IO)
                    (AgentIn d, Agent (RandT g IO) o d)
                    (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d)
runAgentWithDt dt = readerS $ proc (_, (ain, sf)) -> do
  (ao, sf') <- runReaderS_ runAgent dt -< (ain, sf)
  returnA -< (ao, sf')

runAgent :: RandomGen g
          => SF (RandT g IO)
              (AgentIn d, Agent (RandT g IO) o d)
              (AgentOut (RandT g IO) o d, Agent (RandT g IO) o d)
runAgent = arrM (\(ain, sf) -> unMSF sf ain)

   
-- AgentIn TX related
isRequestTx :: AgentIn d -> Bool
isRequestTx = isJust . aiRequestTx

requestTxData :: AgentIn d -> DataFlow d
requestTxData = fromJust . aiRequestTx

requestTxIn :: AgentIn d -> Maybe (DataFlow d)
requestTxIn = aiRequestTx

-- AgentOut TX related
requestTx :: DataFlow d 
          -> AgentTX m o d
          -> AgentOut m o d 
          -> AgentOut m o d
requestTx df txSf ao = ao { aoRequestTx = Just (df, txSf) }

acceptTX :: d 
         -> AgentTX m o d
         -> AgentOut m o d 
         -> AgentOut m o d
acceptTX d txSf ao = ao { aoAcceptTx = Just (d, txSf) }

-- AgentTXOut related
agentTXOut :: AgentTXOut m o d
agentTXOut = AgentTXOut
  {
    aoTxData    = Nothing
  , aoTxCommit  = Nothing
  , aoTxAbort   = False
  }

txDataOut :: d -> AgentTXOut m o d -> AgentTXOut m o d
txDataOut d aoTx = aoTx { aoTxData = Just d }

commitTx :: AgentOut m o d 
         -> AgentTXOut m o d 
         -> AgentTXOut m o d
commitTx ao aoTx = aoTx { aoTxCommit = Just (ao, Nothing) }

commitTxWithCont :: AgentOut m o d 
                 -> Agent m o d
                 -> AgentTXOut m o d 
                 -> AgentTXOut m o d
commitTxWithCont ao sf aoTx = aoTx { aoTxCommit = Just (ao, Just sf) }

abortTx :: AgentTXOut m o d -> AgentTXOut m o d
abortTx aoTx = aoTx { aoTxAbort = True}

-- AgentTXIn related
txDataIn :: AgentTXIn d -> d
txDataIn = fromJust . aiTxData

hasTxDataIn :: AgentTXIn d -> Bool
hasTxDataIn = isJust . aiTxData

isCommitTX :: AgentTXIn d -> Bool
isCommitTX = aiTxCommit

isAbortTX :: AgentTXIn d -> Bool
isAbortTX = aiTxAbort

-- agent related
agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut m o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId        = aid
  , aiData      = []

  , aiRequestTx = Nothing
  }

agentOut :: AgentOut m o d
agentOut = agentOut_ Nothing

agentOutObs :: o -> AgentOut m o d
agentOutObs o = agentOut_ (Just o)

agentOut_ :: Maybe o -> AgentOut m o d
agentOut_ o = AgentOut {
  aoData        = []
, aoObservable  = o

, aoRequestTx   = Nothing
, aoAcceptTx    = Nothing
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