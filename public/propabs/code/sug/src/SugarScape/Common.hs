module SugarScape.Common 
  ( sugObservableFromState
  
  , BestCellMeasureFunc
  , selectBestCells
  , bestCellFunc
  , bestMeasureSugarLevel

  , unoccupiedNeighbourhoodOfNeighbours

  , cellOccupier
  , randomAgent
  ) where

import Control.Monad.Random

import Data.Maybe
import Data.List

import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Model

------------------------------------------------------------------------------------------------------------------------
-- GENERAL FUNCTIONS, independent of monadic / non-monadic implementation
------------------------------------------------------------------------------------------------------------------------
sugObservableFromState :: SugAgentState -> SugAgentObservable
sugObservableFromState s = SugAgentObservable
  { sugObsCoord    = sugAgCoord s 
  , sugObsVision   = sugAgVision s
  }

type BestCellMeasureFunc = (SugEnvCell -> Double) 

bestCellFunc :: BestCellMeasureFunc
bestCellFunc = bestMeasureSugarLevel

selectBestCells :: BestCellMeasureFunc
                -> Discrete2dCoord
                -> [(Discrete2dCoord, SugEnvCell)]
                -> [(Discrete2dCoord, SugEnvCell)]
selectBestCells measureFunc refCoord cs = bestShortestdistanceManhattanCells
  where
    cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
    bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
    bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

    shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d refCoord (fst c1)) (distanceManhattanDisc2d refCoord (fst c2))) bestCells
    shortestdistanceManhattan = distanceManhattanDisc2d refCoord (fst $ head shortestdistanceManhattanBestCells)
    bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d refCoord) . fst) shortestdistanceManhattanBestCells

unoccupiedNeighbourhoodOfNeighbours :: Discrete2dCoord 
                                    -> SugEnvironment
                                    -> [(Discrete2dCoord, SugEnvCell)]
unoccupiedNeighbourhoodOfNeighbours coord e 
    = filter (isNothing . sugEnvOccupier . snd) nncsUnique
  where
    ncs = neighbours coord False e
    -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
    nncsDupl = foldr (\(coord', _) acc -> neighbours coord' False e ++ acc) ncs ncs
    -- NOTE: the nncs are not unique, remove duplicates
    nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl

bestMeasureSugarLevel :: BestCellMeasureFunc
bestMeasureSugarLevel = sugEnvSugarLevel

cellOccupier :: AgentId -> SugAgentState -> SugEnvCellOccupier
cellOccupier aid s = SugEnvCellOccupier 
  { sugEnvOccId     = aid
  , sugEnvOccWealth = sugAgSugarLevel s
  }

randomAgent :: RandomGen g  
            => (AgentId, Discrete2dCoord)
            -> (AgentId -> SugAgentState -> SugAgent g)
            -> (SugAgentState -> SugAgentState)
            -> Rand g (SugAgentDef g, SugAgentState)
randomAgent (agentId, coord) beh sup = do
  -- NOTE: need to split here otherwise agents would end up with the same random-values when not already splitting in the calling function
  _rng <- getSplit

  randSugarMetab     <- getRandomR sugarMetabolismRange
  randVision         <- getRandomR  visionRange
  randSugarEndowment <- getRandomR sugarEndowmentRange
  
  let s = SugAgentState {
    sugAgCoord      = coord
  , sugAgSugarMetab = randSugarMetab
  , sugAgVision     = randVision
  , sugAgSugarLevel = randSugarEndowment
  , sugAgSugarInit  = randSugarEndowment
  }

  let s'   = sup s
  let adef = AgentDef {
    adId       = agentId
  , adBeh      = beh agentId s'
  }

  return (adef, s')