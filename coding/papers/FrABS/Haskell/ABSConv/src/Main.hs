{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Maybe
import qualified Data.Map as Map
import System.IO

import Control.Monad.Random
import Control.Monad.Reader
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
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o

  , aoTxBegin     :: Maybe (DataFlow d)
  , aoTxData      :: Maybe d
  , aoTxCommit    :: Bool
  } deriving (Show)

type Agent m o d          = SF m (AgentIn d) (AgentOut o d)
type AgentObservable o    = (AgentId, Maybe o)

data ConvTestObservable   = AgentWealth Double deriving (Show)

data ConvTestMsg          = OfferingRequest Double
                          | OfferingRefuse 
                          | OfferingAccept Double
                          deriving (Show, Eq)

type ConvTestAgentIn      = AgentIn ConvTestMsg
type ConvTestAgentOut     = AgentOut ConvTestObservable ConvTestMsg
type ConvTestAgent g      = Agent (Rand g) ConvTestObservable ConvTestMsg

type ConvTestEnv          = [AgentId]

agentCount :: Int
agentCount = 1

rngSeed :: Int
rngSeed = 42

t :: Time
t = 10

dt :: DTime
dt = 1.0

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let g = mkStdGen rngSeed

  let as = initTestAgents agentCount

  obss <- runSimulationUntil g t dt as

  mapM_ (\obs -> mapM_ (putStrLn . show) obs) obss

initTestAgents :: RandomGen g 
               => Int 
               -> [(AgentId, ConvTestAgent g)]
initTestAgents n = map (\ai -> (ai, (testAgent 0 env))) env
  where
    env = [0..n-1]

testAgent :: RandomGen g 
          => Double
          -> ConvTestEnv
          -> ConvTestAgent g
testAgent _w0 _env = switch checkTxAgent txCont
  where
    checkTxAgent :: SF (Rand g) ConvTestAgentIn (ConvTestAgentOut, Event c)
    checkTxAgent = undefined

    txCont :: c -> ConvTestAgent g
    txCont = undefined

requestTxAgent :: RandomGen g 
               => Double
               -> ConvTestEnv
               -> ConvTestAgent g
requestTxAgent _w0 env = proc _ain -> do
  raid <- arrM_ (getRandomR (0, length env - 1)) -< ()
  rask <- arrM_ (getRandomR (50, 100)) -< ()

  returnA -< beginTx (raid, OfferingRequest rask) agentOut

-------------------------------------------------------------------------------
runSimulationUntil :: RandomGen g
                   => g
                   -> Time 
                   -> DTime
                   -> [(AgentId, Agent (Rand g) o d)]
                   -> IO [[AgentObservable o]]
runSimulationUntil g t dt aiMsfs = do
  let steps = floor $ t / dt
  let ticks = replicate steps ()

  let ais = map fst aiMsfs
  let msfs = map snd aiMsfs
  let ains  = map agentIn ais

  let aossM = embed (parSimulation msfs ains) ticks

  let readerM = runReaderT aossM dt
  let aoss = evalRand readerM g

  let aobs = map (\aos -> map (\(aid, ao) -> (aid, agentObservable ao)) aos) aoss

  return aobs

parSimulation :: RandomGen g
              => [Agent (Rand g) o d] 
              -> [AgentIn d] 
              -> SF (Rand g)
                  ()
                  [(AgentId, AgentOut o d)]
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: RandomGen g
                     => SF (Rand g) 
                          ((), ([Agent (Rand g) o d], [AgentIn d]))
                          ([(AgentId, AgentOut o d)], ([Agent (Rand g) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      aiosMsfs <- arrM (\(msfs, ains) -> mapM runAgent (zip ains msfs)) -< (msfs, ains)

      let aios = map fst aiosMsfs
      let msfs' = map snd aiosMsfs

      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeData ains' aios

      returnA -< (aios, (msfs', ains''))

    runAgent :: RandomGen g
             => (AgentIn d, Agent ((Rand g)) o d)
             -> ReaderT DTime (Rand g)
                  ((AgentId, AgentOut o d), Agent ((Rand g)) o d) 
    runAgent (ain, msf) = do
      let aid = agentId ain
      (ao, msf') <- unMSF msf ain
      return ((aid, ao), msf')

      {-
runAgents :: Monad m 
          => SF m 
              ([Agent m o d e], [AgentIn o d e], e) 
              ([Agent m o d e], [AgentOut m o d e], [e])
runAgents = readerS $ proc (dt, (sfs, ins, e)) -> do
    let asIns        = zipWith (\sf ain -> (dt, (sf, ain, e))) sfs ins

    arets <- mapMSF (runReaderS runAgent) -< asIns

    let (aos, aEsSfs) = unzip arets
        (es,  sfs')   = unzip aEsSfs

    returnA -< (sfs', aos, es)

  where
    runAgent :: Monad m 
            => SF m 
                  (Agent m o d e, AgentIn o d e, e)
                  (AgentOut m o d e, (e, Agent m o d e))
    runAgent = runStateSF_ runAgentAux agentOut
      where
        runAgentAux :: Monad m
                    => SF (StateT (AgentOut m o d e) m) 
                        (Agent m o d e, AgentIn o d e, e) 
                        (e, Agent m o d e)
        runAgentAux = arrM (\(sf, ain, e) -> unMSF sf (ain, e))

runStateSF_ :: Monad m => SF (StateT s m) a b -> s -> SF m a (s, b)
runStateSF_ sf = runStateS_ $ liftMSFPurer commute sf

-- deep magic going on as well...
commute :: Monad m => ReaderT r (StateT s m) a -> StateT s (ReaderT r m) a
commute rt = 
  StateT (\s -> 
    ReaderT (\r -> let st = runReaderT rt r
                    in runStateT st s))
-}

isBeginTx :: AgentIn d -> Bool
isBeginTx = isJust . aiTxBegin

beginTxData :: AgentIn d -> DataFlow d
beginTxData = fromJust . aiTxBegin

txDataIn :: AgentIn d -> d
txDataIn = fromJust . aiTxData

isCommitTx :: AgentIn d -> Bool
isCommitTx = aiTxCommit


beginTx :: DataFlow d -> AgentOut o d -> AgentOut o d
beginTx df ao = ao { aoTxBegin = Just df }

txDataOut :: d -> AgentOut o d -> AgentOut o d
txDataOut d ao = ao { aoTxData = Just d }

commitTx :: AgentOut o d -> AgentOut o d
commitTx ao = ao { aoTxCommit = True }


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
