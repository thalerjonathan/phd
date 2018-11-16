{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Common
  ( SugarScapeAgent
  , AgentAction
  
  , EventHandler

  , SiteMeasureFunc

  , sugObservableFromState
  , selectBestSites
  , selectSiteMeasureFunc
  , combatSiteMeasure
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
  
  , unoccupiedNeighbourhoodOfNeighbours

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

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import Data.MonadicStreamFunction
import Data.List

import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario

type SugarScapeAgent g = SugarScapeScenario -> AgentId -> SugAgentState -> SugAgentMSF g
type AgentAction g out = StateT SugAgentState (SugAgentMonadT g) out

type EventHandler g = MSF (StateT SugAgentState (SugAgentMonadT g)) (ABSEvent SugEvent) (SugAgentOut g)

type SiteMeasureFunc = SugEnvSite -> Double

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
  }

selectSiteMeasureFunc :: SugarScapeScenario -> SugAgentState -> SiteMeasureFunc
selectSiteMeasureFunc params as
  | spSpiceEnabled params = sugarSpiceSiteMeasure as
  | otherwise             = sugarSiteMeasure

-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
combatSiteMeasure :: SugarScapeScenario -> Double -> SiteMeasureFunc
combatSiteMeasure _params combatReward site = combatWealth + sug + spi
  where
    victim       = fromJust $ sugEnvSiteOccupier site
    victimWealth = sugEnvOccSugarWealth victim + sugEnvOccSpiceWealth victim
    combatWealth = min victimWealth combatReward

    pol          = sugEnvSitePolutionLevel site
    sug          = sugEnvSiteSugarLevel site / (1 + pol)
    spi          = sugEnvSiteSpiceLevel site / (1 + pol)

-- See page 97, The Agent Welfare Function and Appendix C (Example makes it quite clear)
-- The agent welfare function itself computes whether the agent requires more 
-- sugar or more spice, depending on the respective metabolisms. 
-- Now we apply this welfare function to compute a measure for the site which means
-- we compute the potential welfare when the agent is on that site, thus we
-- add the sites sugar / spice to the respective parts of the equation.
-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
sugarSpiceSiteMeasure :: SugAgentState -> SiteMeasureFunc
sugarSpiceSiteMeasure as site = agentWelfareChange sug spi w1 w2 m1 m2
  where
    m1 = fromIntegral $ sugAgSugarMetab as
    m2 = fromIntegral $ sugAgSpiceMetab as
    w1 = sugAgSugarLevel as
    w2 = sugAgSpiceLevel as

    pol = sugEnvSitePolutionLevel site
    sug = sugEnvSiteSugarLevel site / (1 + pol)
    spi = sugEnvSiteSpiceLevel site / (1 + pol)

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
mrsM = do
  s <- get
  return $ mrsState s

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

-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
sugarSiteMeasure :: SiteMeasureFunc
sugarSiteMeasure site = sug / (1 + pol)
  where
    sug = sugEnvSiteSugarLevel site
    pol = sugEnvSitePolutionLevel site

selectBestSites :: SiteMeasureFunc
                -> Discrete2dCoord
                -> [(Discrete2dCoord, SugEnvSite)]
                -> [(Discrete2dCoord, SugEnvSite)]
selectBestSites measureFunc refCoord cs = bestShortestdistanceManhattanCells
  where
    cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
    bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
    bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

    shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d refCoord (fst c1)) (distanceManhattanDisc2d refCoord (fst c2))) bestCells
    shortestdistanceManhattan = distanceManhattanDisc2d refCoord (fst $ head shortestdistanceManhattanBestCells)
    bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d refCoord) . fst) shortestdistanceManhattanBestCells

unoccupiedNeighbourhoodOfNeighbours :: Discrete2dCoord 
                                    -> SugEnvironment
                                    -> [(Discrete2dCoord, SugEnvSite)]
unoccupiedNeighbourhoodOfNeighbours coord e 
    = filter (isNothing . sugEnvSiteOccupier . snd) nncsUnique
  where
    ncs = neighbours coord False e
    -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
    nncsDupl = foldr (\(coord', _) acc -> neighbours coord' False e ++ acc) ncs ncs
    -- NOTE: the nncs are not unique, remove duplicates
    nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl

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
  envLift $ changeCellAtM coord cell'

updateSiteWithOccupier :: RandomGen g
                       => AgentId
                       -> AgentAction g ()
updateSiteWithOccupier aid = do
  (coord, cell) <- agentCellOnCoord
  occ           <- occupierM aid
  let cell' = cell { sugEnvSiteOccupier = Just occ }
  envLift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                => AgentAction g (Discrete2dCoord, SugEnvSite)
agentCellOnCoord = do
  coord <- agentProperty sugAgCoord
  cell  <- envLift $ cellAtM coord
  return (coord, cell)

randomAgent :: RandomGen g  
            => SugarScapeScenario
            -> (AgentId, Discrete2dCoord)
            -> SugarScapeAgent g
            -> (SugAgentState -> SugAgentState)
            -> Rand g (SugAgentDef g, SugAgentState)
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
  , sugAgImmuneSystem = [] -- TODO: initialise
  , sugAgDiseases     = [] -- TODO: initialise
  }

  let s'   = f s
      adef = AgentDef {
    adId      = agentId
  , adSf      = asf params agentId s'
  , adInitObs = sugObservableFromState s'
  }

  return (adef, s')

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

randomCultureTag :: RandomGen g
                 => SugarScapeScenario
                 -> Rand g CultureTag
randomCultureTag params = 
  case spCulturalProcess params of 
    Nothing -> return []
    Just n  -> do
      rs <- getRandoms
      return $ take n rs

randomGender :: RandomGen g
             => Double
             -> Rand g AgentGender
randomGender p = do
  r <- getRandom
  if r >= p
    then return Male
    else return Female

randomFertilityRange :: RandomGen g
                     => SugarScapeScenario 
                     -> AgentGender
                     -> Rand g (Int, Int)
randomFertilityRange params Male = do
  from <- getRandomR $ spFertStartRangeMale params
  to   <- getRandomR $ spFertEndRangeMale params
  return (from, to)
randomFertilityRange params Female = do
  from <- getRandomR $ spFertStartRangeFemale params
  to   <- getRandomR $ spFertEndRangeFemale params
  return (from, to)

randomAgentAge :: RandomGen g
               => AgentAgeSpan 
               -> Rand g (Maybe Int)
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