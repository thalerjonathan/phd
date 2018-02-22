{-# LANGUAGE Arrows #-}
module SD where

import System.IO

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import FRP.Yampa

import SIR

type SDEntityId  = Int
type AgentData d = (SDEntityId, d)

data AgentIn d = AgentIn
  { aiId    :: !SDEntityId
  , aiData  :: ![AgentData d]
  } deriving (Show)

data AgentOut o d = AgentOut
  { aoData        :: ![AgentData d]
  , aoObservable  :: !o
  } deriving (Show)

type Agent o d    = SF (AgentIn d) (AgentOut o d)

type SDMsg       = Double
type SDEntityIn  = AgentIn SDMsg

type SDObs       = Maybe Double
type SDEntityOut = AgentOut SDObs SDMsg
type SDEntity    = Agent SDObs SDMsg

totalPopulation :: Double
totalPopulation = 1000

infectedCount :: Double
infectedCount = 1

rngSeed :: Int
rngSeed = 42

dt :: DTime
dt = 0.01

t :: Time
t = 150

susceptibleStockId :: SDEntityId
susceptibleStockId = 0

infectiousStockId :: SDEntityId
infectiousStockId = 1

recoveredStockId :: SDEntityId
recoveredStockId = 2

infectionRateFlowId :: SDEntityId
infectionRateFlowId = 3

recoveryRateFlowId :: SDEntityId
recoveryRateFlowId = 4

sirSD :: IO ()
sirSD = do
  hSetBuffering stdout NoBuffering

  let as = initAgents totalPopulation infectedCount
  let dyns = runSimulation t dt as

  let fileName =  "STEP_3_DATAFLOW_SD_DYNAMICS_" ++ show totalPopulation ++ "agents.m"
  writeAggregatesToFile fileName dyns

runSimulation :: Time 
              -> DTime 
              -> [(SDEntityId, SDEntity)] 
              -> [(Double, Double, Double)]
runSimulation t dt as = dyns
  where
    steps = floor $ t / dt
    dts = replicate steps (dt, Nothing) -- keep input the same as initial one, will be ignored anyway

    sfs = map snd as
    ains = map (\(aid, _) -> agentIn aid) as

    aoss = embed (stepSimulation sfs ains) ((), dts)
    dyns = map extractStockValues aoss

    -- here we are exploiting the fact that the ordering of the agents will not change during execution
    extractStockValues :: [SDEntityOut] -> (Double, Double, Double)
    extractStockValues (susStockAo : infStockAo : recStockAo : _) = (susValue, infValue, recValue)
      where
        susValue = fromJust $ aoObservable susStockAo
        infValue = fromJust $ aoObservable infStockAo
        recValue = fromJust $ aoObservable recStockAo
    extractStockValues _ = (0, 0, 0)

stepSimulation :: [SDEntity] -> [SDEntityIn] -> SF () [SDEntityOut]
stepSimulation sfs ains =
    dpSwitch
      (\_ sfs' -> (zip ains sfs'))
      sfs
      (switchingEvt >>> notYet) -- at time = 0, if we switch immediately we end up in endless switching, so always wait for 'next'
      stepSimulation

  where
    switchingEvt :: SF ((), [SDEntityOut]) (Event [SDEntityIn])
    switchingEvt = proc (_, aos) -> do
      let ais      = map aiId ains
          aios     = zip ais aos
          nextAins = distributeData aios
      returnA -< Event nextAins

------------------------------------------------------------------------------------------------------------------------
-- STOCKS
------------------------------------------------------------------------------------------------------------------------
susceptibleStock :: Double -> SDEntity
susceptibleStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (-infectionRate)

  let ao  = agentOut (Just stockValue)
      ao' = dataFlow (infectionRateFlowId, stockValue) ao

  returnA -< ao'

infectiousStock :: Double -> SDEntity
infectiousStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain
      recoveryRate  = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (infectionRate - recoveryRate)
  
  let ao   = agentOut (Just stockValue)
      ao'  = dataFlow (infectionRateFlowId, stockValue) ao
      ao'' = dataFlow (recoveryRateFlowId, stockValue) ao'
      
  returnA -< ao''

recoveredStock :: Double -> SDEntity
recoveredStock initValue = proc ain -> do
  let recoveryRate = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< recoveryRate

  returnA -< agentOut (Just stockValue)
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- FLOWS
------------------------------------------------------------------------------------------------------------------------
infectionRateFlow :: SDEntity
infectionRateFlow = proc ain -> do

  let susceptible = flowInFrom susceptibleStockId ain 
      infectious  = flowInFrom infectiousStockId ain

      flowValue   = (infectious * contactRate * susceptible * infectivity) / totalPopulation
  
      ao          = agentOut Nothing
      ao'         = dataFlow (susceptibleStockId, flowValue) ao
      ao''        = dataFlow (infectiousStockId, flowValue) ao'

  returnA -< ao''

recoveryRateFlow :: SDEntity
recoveryRateFlow = proc ain -> do

  let infectious = flowInFrom infectiousStockId ain

      flowValue  = infectious / illnessDuration

      ao         = agentOut Nothing
      ao'        = dataFlow (infectiousStockId, flowValue) ao
      ao''       = dataFlow (recoveredStockId, flowValue) ao'

  returnA -< ao''
------------------------------------------------------------------------------------------------------------------------

initAgents :: Double -> Double -> [(SDEntityId, SDEntity)]
initAgents n i = 
    [ (susceptibleStockId, susStockSf)
    , (infectiousStockId, infStockSf)
    , (recoveredStockId, recStockSf)
    , (infectionRateFlowId, infFlowSf)
    , (recoveryRateFlowId, recFlowSf)
    ]
  where
    susStockSf = susceptibleStock (n - i)
    infStockSf = infectiousStock i
    recStockSf = recoveredStock 0
    
    infFlowSf = infectionRateFlow 
    recFlowSf = recoveryRateFlow

dataFlow :: AgentData d -> AgentOut o d -> AgentOut o d
dataFlow df ao = ao { aoData = df : aoData ao }

onDataM :: (Monad m) 
        => (acc -> AgentData d -> m acc) 
        -> AgentIn d 
        -> acc 
        -> m acc
onDataM dHdl ai acc = foldM dHdl acc ds
  where
    ds = aiData ai

flowInFrom :: SDEntityId -> SDEntityIn -> Double
flowInFrom senderId ain = firstValue dsFiltered
  where 
    ds = aiData ain
    dsFiltered = filter ((==senderId) . fst) ds

    firstValue :: [AgentData SDMsg] -> Double
    firstValue [] = 0.0
    firstValue ((_, v) : _) = v

onData :: (AgentData d -> acc -> acc) -> AgentIn d -> acc -> acc
onData dHdl ai a = foldr dHdl a ds
  where
    ds = aiData ai

distributeData :: [(SDEntityId, AgentOut o d)] -> [AgentIn d]
distributeData aouts = map (distributeDataAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllData aouts
    ains = map (\(ai, _) -> agentIn ai) aouts 

    distributeDataAux :: Map.Map SDEntityId [AgentData d]
                      -> AgentIn d
                      -> AgentIn d
    distributeDataAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiData ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (++ msgs) mayReceiverMsgs

        ain' = ain { aiData = msgsEvt }

    collectAllData :: [(SDEntityId, AgentOut o d)] -> Map.Map SDEntityId [AgentData d]
    collectAllData = foldr collectAllDataAux Map.empty
      where
        collectAllDataAux :: (SDEntityId, AgentOut o d)
                              -> Map.Map SDEntityId [AgentData d]
                              -> Map.Map SDEntityId [AgentData d]
        collectAllDataAux (senderId, ao) accMsgs 
            | not $ null msgs = foldr collectAllDataAuxAux accMsgs msgs
            | otherwise = accMsgs
          where
            msgs = aoData ao

            collectAllDataAuxAux :: AgentData d
                                 -> Map.Map SDEntityId [AgentData d]
                                 -> Map.Map SDEntityId [AgentData d]
            collectAllDataAuxAux (receiverId, m) accMsgs = accMsgs'
              where
                msg = (senderId, m)
                mayReceiverMsgs = Map.lookup receiverId accMsgs
                newMsgs = maybe [msg] (\receiverMsgs -> msg : receiverMsgs) mayReceiverMsgs

                -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
                accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)

agentIn :: SDEntityId -> AgentIn d
agentIn aid = AgentIn {
    aiId    = aid
  , aiData  = []
  }

agentOut :: o -> AgentOut o d
agentOut o = AgentOut {
    aoData        = []
  , aoObservable  = o
  }