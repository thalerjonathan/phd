{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Common
  ( SugarScapeAgent
  , BestSiteMeasureFunc

  , sugObservableFromState
  , selectBestSites
  , bestSiteFunc

  , unoccupiedNeighbourhoodOfNeighbours

  , siteOccupier
  , siteUnoccupied
  , siteOccupied

  , unoccupyPosition
  , agentCellOnCoord
  
  , randomAgent

  , observable
  , updateAgentState
  , agentProperty
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import Data.List

import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model

type SugarScapeAgent g = SugarScapeParams -> AgentId -> SugAgentState -> SugAgentSF g
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

siteOccupier :: AgentId -> SugAgentState -> SugEnvSiteOccupier
siteOccupier aid s = SugEnvSiteOccupier 
  { sugEnvOccId = aid
  }

siteOccupied :: SugEnvSite -> Bool
siteOccupied = isJust . sugEnvSiteOccupier

siteUnoccupied :: SugEnvSite -> Bool
siteUnoccupied = not . siteOccupied

unoccupyPosition :: RandomGen g
                 => StateT SugAgentState (SugAgentMonadT g) ()
unoccupyPosition = do
  (coord, cell) <- agentCellOnCoord
  let cell' = cell { sugEnvSiteOccupier = Nothing }
  lift $ lift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                => StateT SugAgentState (SugAgentMonadT g) (Discrete2dCoord, SugEnvSite)
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
randomAgent params (agentId, coord) beh sup = do
  -- NOTE: need to split here otherwise agents would end up with the same random-values when not already splitting in the calling function
  _rng <- getSplit

  randSugarMetab     <- getRandomR $ spSugarMetabolismRange params
  randVision         <- getRandomR $ spVisionRange params
  randSugarEndowment <- getRandomR $ spSugarEndowmentRange params
  ageSpan            <- randomAgentAge $ spAgeSpan params
  randGender         <- randomGender $ spGenderRatio params
  randFertAgeRange   <- randomFertilityRange params randGender

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
  }

  let s'   = sup s
      adef = AgentDef {
    adId = agentId
  , adSf = beh params agentId s'
  }

  return (adef, s')

randomGender :: RandomGen g
             => Double
             -> Rand g AgentGender
randomGender p = do
  r <- getRandom
  if r <= p
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

observable :: (MonadState SugAgentState m, RandomGen g)
           => m (SugAgentOut g)
observable 
  = get >>= \s -> return $ agentOutObservable $ sugObservableFromState s 