module FRP.Chimera.SD.Definitions 
  (
    StockId
  , FlowId

  , Stock
  , Flow
  , SDObservable
  , SDDef

  , runSD

  , createStock
  , createFlow

  , flowInFrom
  , stockInFrom
  , flowOutTo
  , stockOutTo
) where

import System.Random (StdGen, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM.TVar (newTVarIO)

import FRP.Yampa

import FRP.Chimera.Agent.Agent
import FRP.Chimera.Agent.Reactive
import FRP.Chimera.Simulation.Simulation
import FRP.Chimera.Simulation.Init

data SDMsg          = Value Double deriving (Eq, Show)
type SDStockState   = Double
type SDEnvironment  = ()
type StockId        = AgentId
type FlowId         = AgentId

type SDDef          = AgentDef SDStockState SDMsg SDEnvironment 
type SDBehaviour    = ReactiveBehaviourIgnoreEnv SDStockState SDMsg SDEnvironment 
type SDIn           = AgentIn SDStockState SDMsg SDEnvironment 
type SDOut          = AgentOut SDStockState SDMsg SDEnvironment 
type SDObservable   = AgentObservable SDStockState

type Stock          = Double -> SDBehaviour
type Flow           = SDBehaviour

createStock :: AgentId 
              -> SDStockState
              -> Stock
              -> SDDef
createStock stockId stockState stockBeh = AgentDef { 
    adId = stockId
  , adBeh = ignoreEnv (stockBeh stockState)
  , adInitMessages = NoEvent
  }

createFlow :: AgentId 
              -> Flow
              -> SDDef
createFlow flowId flowBeh = AgentDef { 
    adId = flowId
  , adBeh = ignoreEnv flowBeh
  , adInitMessages = NoEvent
  }

flowInFrom :: AgentId -> SDIn -> Double
flowInFrom = valueInFrom

stockInFrom :: AgentId -> SDIn -> Double
stockInFrom = valueInFrom

flowOutTo :: Double -> AgentId -> SDOut -> SDOut
flowOutTo = valueOutTo

stockOutTo :: Double -> AgentId -> SDOut -> SDOut
stockOutTo = valueOutTo

runSD :: [SDDef] -> DTime -> Time -> [(Time, [SDObservable])]
runSD initSdDefs dt t = map (\(t, outs, _) -> (t, outs)) sdObsEnv
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
      , simEnvFold = Nothing
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
filterMessageValue (_, Value v) _ = v

valueInFrom :: AgentId -> SDIn -> Double
valueInFrom senderId ain = onMessageFrom senderId filterMessageValue ain 0.0 

valueOutTo :: Double -> AgentId -> SDOut -> SDOut
valueOutTo value receiverId ao = sendMessage (receiverId, Value value) ao
------------------------------------------------------------------------------------------------------------------------