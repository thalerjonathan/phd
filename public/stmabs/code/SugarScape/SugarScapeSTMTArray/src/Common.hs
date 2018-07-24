module Common 
  (
    agentOut
  , agentOutObservable
  , isDead
  , kill
  , newAgent
  , isObservable

  , sugObservableFromState

  , BestCellMeasureFunc
  , selectBestCells
  , bestCellFunc
  , bestMeasureSugarLevel

  , unoccupiedNeighbourhoodOfNeighbours

  , randomAgent

  , cellOccupier
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Random
import qualified FRP.BearRiver as BR

import           Data.Maybe
import           Data.List

import           Discrete
import           Model

------------------------------------------------------------------------------------------------------------------------
-- GENERAL FUNCTIONS, independent of monadic / non-monadic implementation
------------------------------------------------------------------------------------------------------------------------
agentOut :: SugAgentOut g
agentOut = agentOutAux Nothing

agentOutObservable :: SugAgentObservable 
                   -> SugAgentOut g
agentOutObservable o = agentOutAux $ Just o

agentOutAux :: Maybe SugAgentObservable
            -> SugAgentOut g
agentOutAux mo = SugAgentOut 
  { sugAoKill       = BR.NoEvent
  , sugAoNew        = []
  , sugAoObservable = mo
  }

isDead :: SugAgentOut g -> Bool
isDead ao = BR.isEvent $ sugAoKill ao

kill :: SugAgentOut g -> SugAgentOut g
kill ao = ao { sugAoKill = BR.Event () }

newAgent :: AgentId
         -> SugAgent g
         -> SugAgentOut g
         -> SugAgentOut g
newAgent aid a ao 
  = ao { sugAoNew = (aid, a) : sugAoNew ao }

isObservable :: SugAgentOut g -> Bool
isObservable ao = isJust $ sugAoObservable ao

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
                                    -> STM [(Discrete2dCoord, SugEnvCell)]
unoccupiedNeighbourhoodOfNeighbours coord e = do
  ncs <- neighbours coord False e
  -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
  nncsDupl <- foldM (\acc (coord', _) -> do
                        ncs' <- neighbours coord' False e
                        return $ ncs' ++ acc) ncs ncs
  -- NOTE: the nncs are not unique, remove duplicates
  let nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
  return $ filter (isNothing . sugEnvOccupier . snd) nncsUnique

bestMeasureSugarLevel :: BestCellMeasureFunc
bestMeasureSugarLevel = sugEnvSugarLevel

------------------------------------------------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------------------------------------------------
cellOccupier :: AgentId -> SugAgentState -> SugEnvCellOccupier
cellOccupier aid s = SugEnvCellOccupier 
  { sugEnvOccId     = aid
  , sugEnvOccWealth = sugAgSugarLevel s
  }

randomAgent :: (MonadRandom m, MonadSplit g m, RandomGen g)
            => (AgentId, Discrete2dCoord)
            -> (AgentId -> SugAgentState -> SugAgent g)
            -> (SugAgentState -> SugAgentState)
            -> m (SugAgent g, SugAgentState)
randomAgent (agentId, coord) beh sup = do
  -- NOTE: need to split here otherwise agents would end up with the same random-values when not already splitting in the calling function
  _rng <- getSplit

  randSugarMetab <- getRandomR sugarMetabolismRange
  randVision <- getRandomR  visionRange
  randSugarEndowment <- getRandomR sugarEndowmentRange
   
  let s = SugAgentState {
    sugAgCoord            = coord
  , sugAgSugarMetab       = randSugarMetab
  , sugAgVision           = randVision
  , sugAgSugarLevel       = randSugarEndowment
  , sugAgSugarInit        = randSugarEndowment
  }

  let s'   = sup s
  let abeh = beh agentId s'

  return (abeh, s')