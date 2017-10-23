module FRP.FrABS.SD.Definitions (
  StockId,
  FlowId,
  
  Stock,
  Flow,
  SDObservable,
  SDDef,
  
  runSD,
  
  createStock,
  createFlow,

  flowInFrom,
  stockInFrom,
  flowOutTo,
  stockOutTo
) where

import System.Random (StdGen, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM.TVar (newTVarIO)

import FRP.Yampa

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Reactive
import FRP.FrABS.Simulation.Simulation
import FRP.FrABS.Simulation.Init

data SDMsg = Value Double deriving (Eq, Show)
type SDStockState = Double
type SDEnvironment = ()
type StockId = AgentId
type FlowId = AgentId

type SDDef = AgentDef SDStockState SDMsg SDEnvironment 
type SDBehaviour = ReactiveBehaviourIgnoreEnv SDStockState SDMsg SDEnvironment 
type SDIn = AgentIn SDStockState SDMsg SDEnvironment 
type SDOut = AgentOut SDStockState SDMsg SDEnvironment 
type SDObservable = AgentObservable SDStockState

type Stock = Double -> SDBehaviour
type Flow = SDBehaviour

createStock :: AgentId 
              -> SDStockState
              -> Stock
              -> SDDef
createStock stockId stockState stockBeh = AgentDef { 
    adId = stockId
  , adState = stockState
  , adBeh = ignoreEnv (stockBeh stockState)
  , adInitMessages = NoEvent
  , adConversation = Nothing
  , adRng = dummyRng }

createFlow :: AgentId 
              -> Flow
              -> SDDef
createFlow flowId flowBeh = AgentDef { 
    adId = flowId
  , adState = 0.0 -- NOTE: a flow does not has or use its state, set it to dummy value 0
  , adBeh = ignoreEnv flowBeh
  , adInitMessages = NoEvent
  , adConversation = Nothing
  , adRng = dummyRng }

flowInFrom = valueInFrom
stockInFrom = valueInFrom

flowOutTo = valueOutTo
stockOutTo = valueOutTo

runSD :: [SDDef] -> DTime -> Time -> [[SDObservable]]
runSD initSdDefs dt t = map fst sdObsEnv
  where
    sdObsEnv = simulateTime 
                  initSdDefs 
                  () 
                  params 
                  dt 
                  t

    -- SystemDynamics MUST NOT rely on RNGs at all, so no need to initialize it
    -- SystemDynamics MUST ABSOLUTELY only run Parllel and there is no need to shuffle the agents (=stocks)
    params = SimulationParams {
      simStrategy = Parallel
      , simEnvBehaviour = Nothing
      , simEnvCollapse = Nothing
      , simShuffleAgents = False
      , simRng = dummyRng
      , simIdGen = dummyIdGen
    }

    dummyIdGen = unsafePerformIO  $ newTVarIO 0

------------------------------------------------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------------------------------------------------
-- NOTE: SD is completely deterministic but we need to provide some RNG for the AgentDef
dummyRng :: StdGen
dummyRng = mkStdGen 0

filterMessageValue :: (AgentMessage SDMsg) -> Double -> Double
filterMessageValue (_, Value v) initValue = v

valueInFrom :: AgentId -> SDIn -> Double
valueInFrom senderId ain = onMessageFrom senderId filterMessageValue ain 0.0 

valueOutTo :: Double -> AgentId -> SDOut -> SDOut
valueOutTo value receiverId ao = sendMessage (receiverId, Value value) ao
------------------------------------------------------------------------------------------------------------------------