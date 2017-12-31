{-# LANGUAGE Arrows     #-}
module Main where

import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Random
import Control.Monad.Trans.MSF.Reader
import Data.Maybe
import qualified Data.Map as Map
import FRP.BearRiver

import SIR

type AgentId     = Int
type AgentData d = (AgentId, d)

-- TODO: should we prevent envrionment-modification in TX-functions? 
-- can achieve this by replacing m by Identity monad
type Agent m o d    = SF m (AgentIn d) (AgentOut m o d)
type AgentTX m o d  = SF m (AgentTXIn d) (AgentTXOut m o d)

data AgentIn d = AgentIn
  {
    aiId        :: !AgentId
  , aiRequestTx :: !(Event (AgentData d))
  } deriving (Show)

data AgentOut m o d = AgentOut
  {
    aoObservable  :: !o
  , aoRequestTx   :: !(Event (AgentData d, AgentTX m o d))
  , aoAcceptTx    :: !(Event (d, AgentTX m o d))
  }

data AgentTXIn d = AgentTXIn
  { aiTxData      :: Maybe d
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
  { aoTxData      :: Maybe d
  , aoTxCommit    :: Maybe (AgentOut m o d, Maybe (Agent m o d))
  , aoTxAbort     :: Bool
  }

type SIRMonad g    = Rand g
data SIRMsg        = Contact SIRState deriving (Show, Eq)
type SIRAgentIn    = AgentIn SIRMsg
type SIRAgentOut g = AgentOut (SIRMonad g) SIRState SIRMsg
type SIRAgent g    = Agent (SIRMonad g) SIRState SIRMsg

agentCount :: Int
agentCount = 100

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
  let ass = runSimulation g t dt as

  let dyns = aggregateAllStates ass
  let fileName =  "STEP_6_TX_DYNAMICS_" ++ show agentCount ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> [(AgentId, SIRState)] 
              -> [[SIRState]]
runSimulation g t dt as = map (\aos -> map aoObservable aos) aoss
  where
    steps = floor $ t / dt
    dts = replicate steps ()

    ais = map fst as
    sfs = map (\(_, s) -> sirAgent ais s) as
    ains = map (\(aid, _) -> agentIn aid) as

    aossReader = embed (stepSimulation sfs ains) dts
    aossRand = runReaderT aossReader dt
    aoss = evalRand aossRand g

stepSimulation :: RandomGen g
               => [SIRAgent g]
               -> [SIRAgentIn] 
               -> SF (SIRMonad g) () [SIRAgentOut g]
stepSimulation sfs ains = MSF $ \_ -> do
  res <- mapM (\(ai, sf) -> unMSF sf ai) (zip ains sfs)
  let aos  = fmap fst res
      sfs' = fmap snd res

      ais  = map aiId ains
      aios = zip ais aos

  (aios', sfs'') <- unMSF runTransactions (aios, sfs')

  let aos'  = map snd aios'
      ains' = map agentIn ais
      ct    = stepSimulation sfs'' ains'

  return (aos', ct)

sirAgent :: RandomGen g => [AgentId] -> SIRState -> SIRAgent g
sirAgent ais Susceptible = susceptibleAgent ais
sirAgent _   Infected    = infectedAgent
sirAgent _   Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => [AgentId] -> SIRAgent g
susceptibleAgent ais = 
    switch 
      susceptible
      (const $ infectedAgent)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) SIRAgentIn (SIRAgentOut g, Event ())
    susceptible = proc ain -> do
      infected <- arrM (\ain -> lift $ randomBoolM infectivity) -< ain

      if infected 
        then returnA -< (agentOut Infected, Event ())
        else (do
          makeContact <- occasionallyM (1 / contactRate) () -< ()
          contactId   <- drawRandomElemS                    -< ais

          if isEvent makeContact
            then returnA -< (agentOut Susceptible, NoEvent)
            else returnA -< (agentOut Susceptible, NoEvent))

infectedAgent :: RandomGen g => SIRAgent g
infectedAgent = 
    switch
    infected 
      (const recoveredAgent)
  where
    infected :: RandomGen g => SF (SIRMonad g) SIRAgentIn (SIRAgentOut g, Event ())
    infected = proc ain -> do
      recEvt <- occasionallyM illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      -- note that at the moment of recovery the agent can still infect others
      -- because it will still reply with Infected
      let ao = agentOut a
      returnA -< (ao, recEvt)

recoveredAgent :: RandomGen g => SIRAgent g
recoveredAgent = arr (const $ agentOut Recovered)

drawRandomElemS :: MonadRandom m => SF m [a] a
drawRandomElemS = proc as -> do
  r <- getRandomRS ((0, 1) :: (Double, Double)) -< ()
  let len = length as
  let idx = (fromIntegral $ len) * r
  let a =  as !! (floor idx)
  returnA -< a

randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

initAgents :: Int -> Int -> [(AgentId, SIRState)]
initAgents n i = sus ++ inf
  where
    sus = map (\ai -> (ai, Susceptible)) [0..n-i-1]
    inf = map (\ai -> (ai, Infected)) [n-i..n-1]

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId        = aid
  , aiRequestTx = NoEvent
  }

agentOut :: Monad m => o -> AgentOut m o d
agentOut o = AgentOut {
    aoObservable  = o
  , aoRequestTx   = NoEvent
  , aoAcceptTx    = NoEvent
  }

runTransactions :: Monad m
                => SF m
                    ([(AgentId, AgentOut m o d)], [Agent m o d])
                    ([(AgentId, AgentOut m o d)], [Agent m o d])
runTransactions = proc (aios, sfs) -> do
    let els = zip aios sfs
    let m = foldr (\((aid, ao), sf) m' -> Map.insert aid (ao, sf) m') Map.empty els
    m' <- runTransactionsAux -< (els, m)
    let ml = Map.toList m'
    let aiosMsfs@(_, _) = foldr (\(aid, (ao, sf)) (accAio, accSf) -> ((aid, ao) : accAio, sf : accSf)) ([], []) ml
    
    -- when there are still TX requests then run them recursively
    let hasTx = any (\(_, (ao, _)) -> isEvent $ aoRequestTx ao) ml
    if hasTx 
      then runTransactions -< aiosMsfs
      else returnA -< aiosMsfs

  where
    runTransactionsAux :: Monad m
                        => SF m
                            (([((AgentId, AgentOut m o d), Agent m o d)]),
                              (Map.Map AgentId (AgentOut m o d, Agent m o d)))
                            (Map.Map AgentId (AgentOut m o d, Agent m o d))
    runTransactionsAux = proc (els, m) -> do
      if null els
        then returnA -< m
        else (do 
          let e@((_, ao), _) = head els
          if (isEvent $ aoRequestTx ao)
            then (do
              m' <- runTxPair -< (e, m)
              runTransactionsAux -< (tail els, m'))
            else runTransactionsAux -< (tail els, m))

    -- care must be taken if two agents want to start a TX with each other at the same time
    -- note that we allow agents to transact with themselves
    runTxPair :: Monad m
              => SF m
                  (((AgentId, AgentOut m o d), Agent m o d),
                    (Map.Map AgentId (AgentOut m o d, Agent m o d)))
                  (Map.Map AgentId (AgentOut m o d, Agent m o d))
    runTxPair = proc (((sAid, sAo0), sSf0), m) -> do
      let ((rAid, d), sTxSf) = fromEvent $ aoRequestTx sAo0
      let mayReceiver = Map.lookup rAid m

      if isNothing mayReceiver
        -- target of the TX request not found, ignoring TX
        then (do
          -- transaction receiver not found, ignoring TX request
          -- set tx-request in agentout to nothing
          let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
          returnA -< m')
        else (do
          let (_, rSf0) = fromJust mayReceiver
          
          let rAin = (agentIn rAid) { aiRequestTx = Event (sAid, d) }

          -- ignoring the sf of this run makes it as it has never happened,
          (rAo', _) <- runAgentWithDt 0 -< (rSf0, rAin) 

          let acceptTxEvt = aoAcceptTx rAo'
          if isEvent acceptTxEvt
            -- the request has been turned down, no TX
            then (do
              -- transaction request turned down / ignored by receiver
              -- set tx-request in agentout to nothing
              let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
              returnA -< m')
            else (do
              let (d, rTxSf) = fromEvent acceptTxEvt

              let rTxAo0 = AgentTXOut {
                aoTxData      = Just d
              , aoTxCommit    = Nothing
              , aoTxAbort     = False
              }

              mayTx <- runTx -< ((sTxSf, sSf0), (rTxAo0, rTxSf, rSf0))
              if isNothing mayTx
                then (do
                  -- transaction aborted
                  -- set tx-request in agentout to nothing
                  let m' = Map.insert sAid (sAo0 { aoRequestTx = NoEvent }, sSf0) m
                  returnA -< m')
                else (do
                  -- transaction finished, committing
                  let ((sAo', sSf'), (rAo', rSf')) = fromJust mayTx

                  --printDebugS -< ("aid1 = " ++ show aid1 ++ ", ao1 = " ++ show ao1)
                  --printDebugS -< ("aid2 = " ++ show aid2 ++ ", ao2 = " ++ show ao2)
                  
                  let m' = Map.insert sAid (sAo', sSf') m
                  let m'' = Map.insert rAid (rAo', rSf') m'
                  
                  -- transaction committed...
                  returnA -< m'')))

runTx :: Monad m
      => SF m
            ((AgentTX m o d, Agent m o d),
            (AgentTXOut m o d, AgentTX m o d, Agent m o d))
          (Maybe
            ((AgentOut m o d, Agent m o d), 
              (AgentOut m o d, Agent m o d)))
runTx = proc ((sTxSf0, sSf), (rTxAo0, rTxSf0, rSf)) -> do
  let sTxAin = AgentTXIn {
      aiTxData      = aoTxData rTxAo0
    , aiTxCommit    = isJust $ aoTxCommit rTxAo0
    , aiTxAbort     = aoTxAbort rTxAo0
    }

  (sTxAo, sTxSf') <- runAgentTx -< (sTxSf0, sTxAin)
  
  let rTxAin = AgentTXIn {
    aiTxData      = aoTxData sTxAo
  , aiTxCommit    = isJust $ aoTxCommit sTxAo
  , aiTxAbort     = aoTxAbort sTxAo
  }

  (rTxAo', rTxSf') <- runAgentTx -< (rTxSf0, rTxAin)
  
  -- either one aborts the TX, abort the whole TX
  if aoTxAbort sTxAo || aoTxAbort rTxAo'
    then returnA -< Nothing
    else (do
      -- if both commit, we commit the TX as a whole
      -- otherwise we continue with another TX step
      if (isJust $ aoTxCommit sTxAo) && (isJust $ aoTxCommit rTxAo')
        then (do
          let (sAo, maySsf) = fromJust $ aoTxCommit sTxAo
          let (rAo, mayRsf) = fromJust $ aoTxCommit rTxAo'
          let sSf' = fromMaybe sSf maySsf
          let rSf' = fromMaybe rSf mayRsf
          
          returnA -< Just ((sAo, sSf'), (rAo, rSf')))
        -- if not both commit we assume that another TX step is required.
        else runTx -< ((sTxSf', sSf), (rTxAo', rTxSf', rSf)))

runAgentWithDt :: Monad m
               => Double
               -> SF m
                    (Agent m o d, AgentIn d)
                    (AgentOut m o d, Agent m o d)
runAgentWithDt dt = readerS $ proc (_, (sf, ain)) -> do
    (ao, sf') <- runReaderS_ runAgent dt -< (sf, ain)
    returnA -< (ao, sf')
  where
    runAgent = arrM (\(sf, ain) -> do
      (ao, sf') <- unMSF sf ain
      return (ao, sf'))

-- NOTE: TXs always run with dt = 0
runAgentTx :: Monad m
           => SF m
                (AgentTX m o d, AgentTXIn d)
                (AgentTXOut m o d, AgentTX m o d)
runAgentTx = readerS $ proc (_, (txSf, txIn)) -> do
  (txOut, txSf') <- runReaderS_ (arrM (\(txSf, txIn) -> unMSF txSf txIn)) 0 -< (txSf, txIn)
  returnA -< (txOut, txSf')