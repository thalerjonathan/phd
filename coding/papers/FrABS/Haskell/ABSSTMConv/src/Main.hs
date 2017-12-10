{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.Map as Map
import System.IO

import Control.Monad.Random
import Control.Monad.Reader
import Control.Concurrent.STM
import Control.Concurrent.STM.Stats
import FRP.BearRiver

type AgentId = Int
type DataFlow d = (AgentId, d)

data AgentIn d = AgentIn
  {
    aiId    :: AgentId
  , aiData  :: [DataFlow d]
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o
  } deriving (Show)

type Agent m o d        = SF m (AgentIn d) (AgentOut o d)
type AgentObservable o  = (AgentId, Maybe o)

data STMTestObservable  = AgentWealth Double deriving (Show)

data STMTestMsg         = ConvInit (TMVar STMTestMsg)
                        | ConvEnd 

type STMTestAgentIn     = AgentIn STMTestMsg
type STMTestAgentOut    = AgentOut STMTestObservable STMTestMsg
type STMTestAgent g     = Agent (RandT g STM) STMTestObservable STMTestMsg

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
  dumpSTMStats

initTestAgents :: RandomGen g 
               => Int 
               -> [(AgentId, STMTestAgent g)]
initTestAgents n = map (\ai -> (ai, (testAgent ais 0))) ais
  where
    ais = [0..n-1]

testAgent :: RandomGen g 
          => [AgentId]
          -> Double
          -> STMTestAgent g
testAgent agents w0 = proc _ain -> do
  rec
    w' <- iPre w0 -< w
    let w = w' + 0

  returnA -< agentOutObs (AgentWealth w)

startConversation :: STMTestMsg -> STM ()
startConversation msg = do
  v <- newTMVar msg
  return ()

-------------------------------------------------------------------------------
runSimulationUntil :: RandomGen g
                   => g
                   -> Time 
                   -> DTime
                   -> [(AgentId, Agent (RandT g STM) o d)]
                   -> IO [[AgentObservable o]]
runSimulationUntil g t dt aiMsfs = do
  let steps = floor $ t / dt
  let ticks = replicate steps ()

  let ais = map fst aiMsfs
  let msfs = map snd aiMsfs
  let ains  = map agentIn ais

  let aossM = embed (parSimulation msfs ains) ticks

  let readerM = runReaderT aossM dt
  let randM = evalRandT readerM g
  -- aoss <- atomically randM
  aoss <- trackSTM randM

  let aobs = map (\aos -> map (\(aid, ao) -> (aid, agentObservable ao)) aos) aoss

  return aobs

parSimulation :: RandomGen g
              => [Agent (RandT g STM) o d] 
              -> [AgentIn d] 
              -> SF (RandT g STM)
                  ()
                  [(AgentId, AgentOut o d)]
parSimulation msfs0 ains0 = loopPre (msfs0, ains0) parSimulationAux
  where
    parSimulationAux :: RandomGen g
                     => SF (RandT g STM) 
                          ((), ([Agent (RandT g STM) o d], [AgentIn d]))
                          ([(AgentId, AgentOut o d)], ([Agent (RandT g STM) o d], [AgentIn d]))
    parSimulationAux = proc (_, (msfs, ains)) -> do
      aiosMsfs <- arrM (\(msfs, ains) -> mapM runAgent (zip ains msfs)) -< (msfs, ains)

      let aios = map fst aiosMsfs
      let msfs' = map snd aiosMsfs

      let ains' = map (\ai -> agentIn $ agentId ai) ains 
      let ains'' = distributeData ains' aios

      returnA -< (aios, (msfs', ains''))

    runAgent :: RandomGen g
             => (AgentIn d, Agent ((RandT g STM)) o d)
             -> ReaderT DTime (RandT g STM)
                  ((AgentId, AgentOut o d), Agent ((RandT g STM)) o d) 
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

agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId    = aid
  , aiData  = []
  }

agentOut :: AgentOut o d
agentOut = AgentOut {
    aoData        = []
  , aoObservable  = Nothing
  }

agentOutObs :: o -> AgentOut o d
agentOutObs o = AgentOut {
    aoData        = []
  , aoObservable  = Just o
  }

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
