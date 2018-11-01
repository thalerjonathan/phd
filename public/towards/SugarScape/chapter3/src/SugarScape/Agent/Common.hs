{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Common
  ( SugarScapeAgent
  , AgentAction
  
  , EventHandler

  , BestSiteMeasureFunc

  , sugObservableFromState
  , selectBestSites
  , bestSiteFunc

  , unoccupiedNeighbourhoodOfNeighbours

  , occupier
  , siteOccupier
  , siteUnoccupied
  , siteOccupied

  , unoccupyPosition
  , agentCellOnCoord
  
  , randomAgent

  , agentOutObservable
  , agentOutObservableM
  , updateAgentState
  , agentProperty
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import Data.MonadicStreamFunction
import Data.List

import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model

type SugarScapeAgent g = SugarScapeParams -> AgentId -> SugAgentState -> SugAgentMSF g
type AgentAction g out = StateT SugAgentState (SugAgentMonadT g) out

type EventHandler g = MSF (StateT SugAgentState (SugAgentMonadT g)) (ABSEvent SugEvent) (SugAgentOut g)

type BestSiteMeasureFunc = (SugEnvSite -> Double) 

sugObservableFromState :: SugAgentState -> SugAgentObservable
sugObservableFromState s = SugAgentObservable
  { sugObsCoord    = sugAgCoord s 
  , sugObsVision   = sugAgVision s
  , sugObsAge      = sugAgAge s 
  , sugObsSugLvl   = sugAgSugarLevel s
  , sugObsSugMetab = sugAgSugarMetab s
  , sugObsGender   = sugAgGender s
  }

bestSiteFunc :: SugarScapeParams -> BestSiteMeasureFunc
bestSiteFunc params
    | diffusionActive = bestSugarPolutionRatio
    | otherwise       = bestSugarLevel 
  where
    diffusionActive = case spPolutionFormation params of
                        NoPolution -> False
                        _          -> True

bestSugarLevel :: BestSiteMeasureFunc
bestSugarLevel = sugEnvSiteSugarLevel

bestSugarPolutionRatio :: BestSiteMeasureFunc
bestSugarPolutionRatio c 
    = s / (1 + p)
  where
    s = sugEnvSiteSugarLevel c
    p = sugEnvSitePolutionLevel c

selectBestSites :: BestSiteMeasureFunc
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

occupier :: AgentId -> SugEnvSiteOccupier
occupier aid = SugEnvSiteOccupier { 
    sugEnvOccId = aid
  }

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
  lift $ lift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                => AgentAction g (Discrete2dCoord, SugEnvSite)
agentCellOnCoord = do
  coord <- agentProperty sugAgCoord
  cell  <- lift $ lift $ cellAtM coord
  return (coord, cell)

randomAgent :: RandomGen g  
            => SugarScapeParams
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

  let initSugar = fromIntegral randSugarEndowment
      s = SugAgentState {
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
  }

  let s'   = f s
      adef = AgentDef {
    adId      = agentId
  , adSf      = asf params agentId s'
  , adInitObs = sugObservableFromState s'
  }

  return (adef, s')

randomCultureTag :: RandomGen g
                 => SugarScapeParams
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
                     => SugarScapeParams 
                     -> AgentGender
                     -> Rand g (Int, Int)
randomFertilityRange params Male = do
  from <- getRandomR $ spFertStartRangeMen params
  to   <- getRandomR $ spFertEndRangeMen params
  return (from, to)
randomFertilityRange params Female = do
  from <- getRandomR $ spFertStartRangeWoman params
  to   <- getRandomR $ spFertEndRangeWoman params
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

agentOutObservable :: SugAgentState -> SugAgentOut g 
agentOutObservable = agentOut . sugObservableFromState

agentOutObservableM :: (MonadState SugAgentState m, RandomGen g)
                    => m (SugAgentOut g)
agentOutObservableM 
  = get >>= \s -> return $ agentOutObservable s