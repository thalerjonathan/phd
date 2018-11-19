{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Common
  ( SugarScapeAgent
  , AgentAction
  , EventHandler

  , absCtxLift
  , envLift
  , randLift
  , stmLift
  , envRun

  , neighbourAgentIds
  , getSimTime
  , nextAgentId

  , broadcastEvent
  , sendEventTo
  , sendEvents
  
  , agentWelfare
  , agentWelfareM
  , agentWelfareState
  , agentWelfareChange
  , agentWelfareChangeM
  , agentWelfareChangeState
  , mrs
  , mrsM
  , mrsState
  , mrsStateChange

  , occupier
  , occupierM
  , siteOccupier
  , siteUnoccupied
  , siteOccupied

  , unoccupyPosition
  , updateSiteWithOccupier
  , agentCellOnCoord
  
  , randomAgent

  , agentObservable
  , agentObservableM
  , observableTrades
  , updateAgentState
  , agentProperty

  , tagToTribe

  , changeToRedTribe
  , changeToBlueTribe
  ) where

import Data.Maybe

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.STM
import Data.MonadicStreamFunction
import qualified Data.IntMap.Strict as Map -- better performance than normal Map according to hackage page

import SugarScape.Agent.Interface
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario

type SugarScapeAgent g = SugarScapeScenario -> AgentId -> SugAgentState -> SugAgentMSF g
type AgentAction g out = StateT SugAgentState (SugAgentMonadT g) out
type EventHandler g    = MSF (StateT SugAgentState (SugAgentMonadT g)) (ABSEvent SugEvent) (SugAgentOut g)

absCtxLift :: ReaderT (ABSCtx SugEvent) (ReaderT SugEnvironment (RandT g STM)) a -> AgentAction g a
absCtxLift = lift 

envLift :: ReaderT SugEnvironment (RandT g STM) a -> AgentAction g a
envLift = lift . lift

randLift :: RandT g STM a -> AgentAction g a
randLift = lift . lift . lift

stmLift :: STM a -> AgentAction g a
stmLift = lift . lift . lift . lift

envRun :: (SugEnvironment -> STM a) 
       -> AgentAction g a
envRun f = do
  env <- envLift ask 
  stmLift (f env)

broadcastEvent :: AgentId
               -> [AgentId]
               -> SugEvent
               -> AgentAction g ()
broadcastEvent myId rs e
  = mapM_ (\rid -> sendEventTo myId rid e) rs

sendEvents :: AgentId
           -> [(AgentId, SugEvent)]
           -> AgentAction g ()
sendEvents myId
  = mapM_ (uncurry $ sendEventTo myId)

sendEventTo :: AgentId
            -> AgentId
            -> SugEvent
            -> AgentAction g ()
sendEventTo myId receiverId e = do
  msgQsVar <- absCtxMsgQueues <$> absCtxLift ask 
  msgQs    <- stmLift $ readTVar msgQsVar

  let mq = Map.lookup receiverId msgQs
  case mq of
    Nothing -> return () -- not found, ignore (maybe already dead)
    Just q  -> stmLift $ writeTQueue q (DomainEvent (myId, e)) 

neighbourAgentIds :: AgentAction g [AgentId]
neighbourAgentIds = do
    coord <- agentProperty sugAgCoord
    filterNeighbourIds <$> envRun (neighbours coord False)
  where
    filterNeighbourIds :: [Discrete2dCell SugEnvSite] -> [AgentId]
    filterNeighbourIds ns = map (siteOccupier . snd) $ filter (siteOccupied . snd) ns

getSimTime :: AgentAction g Time
getSimTime = absCtxLift $ reader absCtxTime

nextAgentId :: AgentAction g AgentId
nextAgentId = do
  aidVar <- absCtxLift $ reader absCtxIdVar
  stmLift $ stateTVar aidVar (\i -> (i+1, i))

sugObservableFromState :: SugAgentState -> SugAgentObservable
sugObservableFromState as = SugAgentObservable
  { sugObsCoord      = sugAgCoord as 
  , sugObsVision     = sugAgVision as
  , sugObsAge        = sugAgAge as 
  , sugObsSugLvl     = sugAgSugarLevel as
  , sugObsSugMetab   = sugAgSugarMetab as
  , sugObsGender     = sugAgGender as
  , sugObsCultureTag = sugAgCultureTag as
  , sugObsTribe      = sugAgTribe as
  , sugObsSpiLvl     = sugAgSpiceLevel as
  , sugObsSpiMetab   = sugAgSpiceMetab as
  , sugObsTrades     = []
  , sugObsDiseases   = sugAgDiseases as
  }

-- default welfare function, not incorporating change to sugar / spice
agentWelfare :: Double  -- ^ sugar-wealth of agent
             -> Double  -- ^ spice-wealth of agent
             -> Double  -- ^ sugar-metabolism of agent
             -> Double  -- ^ spice-metabolism of agent
             -> Double
agentWelfare = agentWelfareChange 0 0

agentWelfareState :: SugAgentState -> Double
agentWelfareState as = agentWelfareChangeState as 0 0

agentWelfareM :: MonadState SugAgentState m => m Double
agentWelfareM = agentWelfareChangeM 0 0

-- see page 102, internal valuations
mrs :: Double  -- ^ sugar-wealth of agent
    -> Double  -- ^ spice-wealth of agent
    -> Double  -- ^ sugar-metabolism of agent
    -> Double  -- ^ spice-metabolism of agent
    -> Double  -- ^ mrs value: less than 1 the agent values sugar more, and spice otherwise
mrs w1 w2 m1 m2 = (w2 / m2) / (w1 / m1)

mrsM :: MonadState SugAgentState m => m Double
mrsM = mrsState <$> get

mrsStateChange :: SugAgentState 
               -> Double
               -> Double
               -> Double
mrsStateChange as sugarChange spiceChange 
    = mrs (w1 + sugarChange) (w2 + spiceChange) m1 m2
  where
    m1 = fromIntegral $ sugAgSugarMetab as
    m2 = fromIntegral $ sugAgSpiceMetab as
    w1 = sugAgSugarLevel as
    w2 = sugAgSpiceLevel as

mrsState :: SugAgentState -> Double
mrsState as = mrsStateChange as 0 0

-- NOTE: this welfare function includes the ability to calculate the changed
-- welfare of an agent when sugar and spice change - is required for determining
-- the best site to move to when spice is enabled
agentWelfareChange :: Double  -- ^ sugar-change in welfare
                   -> Double  -- ^ spice-change in welfare
                   -> Double  -- ^ sugar-wealth of agent
                   -> Double  -- ^ spice-wealth of agent
                   -> Double  -- ^ sugar-metabolism of agent
                   -> Double  -- ^ spice-metabolism of agent
                   -> Double
agentWelfareChange sugarChange spiceChange w1 w2 m1 m2 
{-
    | isNaN wf = error ("invalid welfare change: w1 = " ++ show w1 ++ 
                        ", w2 = " ++ show w2 ++ 
                        ", m1 = " ++ show m1 ++ 
                        ", m2 = " ++ show m2 ++
                        ", sugarchange = " ++ show sugarChange ++ 
                        ", spiceChange = " ++ show spiceChange)
    | otherwise = wf
    -}
    = (w1Diff ** (m1/mT)) * (w2Diff ** (m2/mT))
  where
    mT = m1 + m2
    w1Diff = max (w1 + sugarChange) 0 -- prevent negative wealth, would result in NaN
    w2Diff = max (w2 + spiceChange) 0 -- prevent negative wealth, would result in NaN

agentWelfareChangeState :: SugAgentState
                        -> Double
                        -> Double
                        -> Double
agentWelfareChangeState as sugarChange spiceChange 
    = agentWelfareChange sugarChange spiceChange w1 w2 m1 m2
  where
    m1 = fromIntegral $ sugAgSugarMetab as
    m2 = fromIntegral $ sugAgSpiceMetab as
    w1 = sugAgSugarLevel as
    w2 = sugAgSpiceLevel as

agentWelfareChangeM :: MonadState SugAgentState m 
                    => Double
                    -> Double
                    -> m Double
agentWelfareChangeM sugarChange spiceChange 
  = state (\s -> (agentWelfareChangeState s sugarChange spiceChange, s))

occupier :: AgentId
         -> SugAgentState
         -> SugEnvSiteOccupier
occupier aid as = SugEnvSiteOccupier { 
    sugEnvOccId          = aid
  , sugEnvOccTribe       = sugAgTribe as
  , sugEnvOccSugarWealth = sugAgSugarLevel as
  , sugEnvOccSpiceWealth = sugAgSpiceLevel as
  , sugEnvOccMRS         = mrsState as
  }

occupierM :: MonadState SugAgentState m
          => AgentId 
          -> m SugEnvSiteOccupier
occupierM aid = get >>= \s -> return $ occupier aid s

siteOccupier :: SugEnvSite -> AgentId
siteOccupier site = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site

siteOccupied :: SugEnvSite -> Bool
siteOccupied = isJust . sugEnvSiteOccupier

siteUnoccupied :: SugEnvSite -> Bool
siteUnoccupied = not . siteOccupied

unoccupyPosition :: RandomGen g
                 => AgentAction g ()
unoccupyPosition = do
  (coord, cell) <- agentCellOnCoord
  let cell' = cell { sugEnvSiteOccupier = Nothing }
  envRun $ changeCellAt coord cell'

updateSiteWithOccupier :: RandomGen g
                       => AgentId
                       -> AgentAction g ()
updateSiteWithOccupier aid = do
  (coord, cell) <- agentCellOnCoord
  occ           <- occupierM aid
  let cell' = cell { sugEnvSiteOccupier = Just occ }
  envRun $ changeCellAt coord cell'

agentCellOnCoord :: RandomGen g
                => AgentAction g (Discrete2dCoord, SugEnvSite)
agentCellOnCoord = do
  coord <- agentProperty sugAgCoord
  cell  <- envRun $ cellAt coord
  return (coord, cell)

randomAgent :: MonadRandom m
            => SugarScapeScenario
            -> (AgentId, Discrete2dCoord)
            -> SugarScapeAgent g
            -> (SugAgentState -> SugAgentState)
            -> m (SugAgentDef g, SugAgentState)
randomAgent params (agentId, coord) asf f = do
  randSugarMetab     <- getRandomR $ spSugarMetabolismRange params
  randVision         <- getRandomR $ spVisionRange params
  randSugarEndowment <- getRandomR $ spSugarEndowmentRange params
  ageSpan            <- randomAgentAge $ spAgeSpan params
  randGender         <- randomGender $ spGenderRatio params
  randFertAgeRange   <- randomFertilityRange params randGender
  randCultureTag     <- randomCultureTag params
  randSpiceEndowment <- getRandomR $ spSpiceEndowmentRange params
  randSpiceMetab     <- getRandomR $ spSpiceMetabolismRange params
  randImmuneSystem   <- randomImmuneSystem params
  randDiseases       <- randomDiseases params

  let initSugar = fromIntegral randSugarEndowment
      initSpice = fromIntegral randSpiceEndowment

  let s = SugAgentState {
    sugAgCoord        = coord
  , sugAgSugarMetab   = randSugarMetab
  , sugAgVision       = randVision
  , sugAgSugarLevel   = initSugar
  , sugAgMaxAge       = ageSpan
  , sugAgAge          = 0
  , sugAgGender       = randGender
  , sugAgFertAgeRange = randFertAgeRange
  , sugAgInitSugEndow = initSugar
  , sugAgChildren     = []
  , sugAgCultureTag   = randCultureTag
  , sugAgTribe        = tagToTribe randCultureTag
  , sugAgSpiceLevel   = initSpice
  , sugAgInitSpiEndow = initSpice
  , sugAgSpiceMetab   = randSpiceMetab
  , sugAgBorrowed     = []
  , sugAgLent         = []
  , sugAgNetIncome    = 0
  , sugAgImmuneSystem = randImmuneSystem
  , sugAgImSysGeno    = randImmuneSystem
  , sugAgDiseases     = randDiseases
  }

  let s'   = f s
      adef = AgentDef {
    adId      = agentId
  , adSf      = asf params agentId s'
  , adInitObs = sugObservableFromState s'
  }

  return (adef, s')

randomDiseases :: MonadRandom m
               => SugarScapeScenario
               -> m [Disease]
randomDiseases params = 
  case spDiseasesEnabled params of 
    Nothing -> return []
    Just (_, _, _, n, masterList) -> 
      randomElemsM n masterList

randomImmuneSystem :: MonadRandom m
                   => SugarScapeScenario
                   -> m ImmuneSystem
randomImmuneSystem params = 
  case spDiseasesEnabled params of 
    Nothing -> return []
    Just (n, _, _, _, _)  -> 
      take n <$> getRandoms

changeToRedTribe :: SugarScapeScenario
                 -> SugAgentState
                 -> SugAgentState
changeToRedTribe params s = s { sugAgTribe      = tagToTribe redTag
                              , sugAgCultureTag = redTag }
  where             
    redTag = case spCulturalProcess params of 
              Nothing -> replicate 10 True -- cultural process is deactivated => select default of 10 to generate different Red tribe
              Just n  -> replicate n True

changeToBlueTribe :: SugarScapeScenario
                  -> SugAgentState
                  -> SugAgentState
changeToBlueTribe params s = s { sugAgTribe     = tagToTribe blueTag
                              , sugAgCultureTag = blueTag }
  where             
    blueTag = case spCulturalProcess params of 
              Nothing -> replicate 10 False -- cultural process is deactivated => select default of 10 to generate different Red tribe
              Just n  -> replicate n False

tagToTribe :: CultureTag
           -> AgentTribe
tagToTribe tag 
    | zeros > ones = Blue
    | otherwise    = Red
  where
    zeros = length $ filter (==False) tag
    ones  = n - zeros  
    n     = length tag

randomCultureTag :: MonadRandom m
                 => SugarScapeScenario
                 -> m CultureTag
randomCultureTag params = 
  case spCulturalProcess params of 
    Nothing -> return []
    Just n  -> 
      take n <$> getRandoms

randomGender :: MonadRandom m
             => Double
             -> m AgentGender
randomGender p = do
  r <- getRandom
  if r >= p
    then return Male
    else return Female

randomFertilityRange :: MonadRandom m
                     => SugarScapeScenario 
                     -> AgentGender
                     -> m (Int, Int)
randomFertilityRange params Male = do
  from <- getRandomR $ spFertStartRangeMale params
  to   <- getRandomR $ spFertEndRangeMale params
  return (from, to)
randomFertilityRange params Female = do
  from <- getRandomR $ spFertStartRangeFemale params
  to   <- getRandomR $ spFertEndRangeFemale params
  return (from, to)

randomAgentAge :: MonadRandom m
               => AgentAgeSpan 
               -> m (Maybe Int)
randomAgentAge Forever         = return Nothing
randomAgentAge (Range from to) = do
  randMaxAge <- getRandomR (from, to)
  return $ Just randMaxAge

updateAgentState :: MonadState SugAgentState m
                 => (SugAgentState -> SugAgentState)
                 -> m ()
updateAgentState = modify

agentProperty :: MonadState SugAgentState m
              => (SugAgentState -> p)
              -> m p
agentProperty = gets

agentObservable :: SugAgentState -> SugAgentOut g 
agentObservable = agentOut . sugObservableFromState

agentObservableM :: (MonadState SugAgentState m, RandomGen g)
                 => m (SugAgentOut g)
agentObservableM 
  = get >>= \s -> return $ agentObservable s

observableTrades :: [TradeInfo] 
                 -> SugAgentOut g 
                 -> SugAgentOut g 
observableTrades trades ao = ao { aoObservable = obs { sugObsTrades = trades }}
  where
    obs = aoObservable ao