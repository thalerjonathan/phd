{-# LANGUAGE Arrows #-}
module Agent 
  (
    sugAgent
  ) where

-- import Control.Monad
-- import Control.Monad.IfElse
import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver
--import Debug.Trace

--import Data.Maybe
--import Data.List

import AgentMonad
import Common
import Environment
import Discrete
import Model
import Random
import Utils

------------------------------------------------------------------------------------------------------------------------
sugAgent :: RandomGen g 
         => AgentId
         -> SugAgentState
         -> SugAgent g
sugAgent aid s0 = feedback s0 (proc (ain, s) -> do
  age      <- time -< ()
  (ao, s') <- arrM (\(age, ain, s) -> lift $ runStateT (chapterII aid ain age) s) -< (age, ain, s)
  returnA -< (ao, s'))

updateAgentState :: RandomGen g
                 => (SugAgentState -> SugAgentState)
                 -> StateT SugAgentState (SugAgentMonadT g) ()
updateAgentState = modify

observable :: RandomGen g
           => StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
observable 
  = get >>= \s -> return $ agentOutObservable $ sugObservableFromState s 

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
chapterII :: RandomGen g 
          => AgentId
          -> SugAgentIn
          -> Time
          -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
chapterII aid _ain age = do
  ao <- agentMetabolism
  ifThenElse
    (isDead ao)
    (return ao)
    (do
      agentMove aid
      ao' <- observable 
      return $ ao <°> ao')

agentDies :: RandomGen g
          => StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
agentDies = do
  unoccupyPosition
  return $ kill agentOut

unoccupyPosition :: RandomGen g
                 => StateT SugAgentState (SugAgentMonadT g) ()
unoccupyPosition = do
  (coord, cell) <- agentCellOnCoord
  let cell' = cell { sugEnvOccupier = Nothing }
  lift $ lift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                 => StateT SugAgentState (SugAgentMonadT g) (Discrete2dCoord, SugEnvCell)
agentCellOnCoord = do
  coord <- gets sugAgCoord
  cell  <- lift $ lift $ cellAtM coord
  return (coord, cell)

agentMetabolism :: RandomGen g
                => StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
agentMetabolism = do
  s <- get
  let (sugarMetab, spiceMetab) = metabolismAmount s

  sugarLevel <- gets sugAgSugarLevel
  spiceLevel <- gets sugAgSpiceLevel

  let newSugarLevel = max 0 (sugarLevel - sugarMetab)
  let newSpiceLevel = max 0 (spiceLevel - spiceMetab)

  updateAgentState (\s' -> s' { sugAgSugarLevel = newSugarLevel, sugAgSpiceLevel = newSpiceLevel })

  -- NOTE: for now the metabolism (and harvest) of spice does not cause any polution
  coord <- gets sugAgCoord
  lift $ lift $ modify $ poluteCell (sugarMetab * polutionMetabolismFactor) coord

  ifThenElseM
    starvedToDeath
    agentDies
    (return agentOut)

starvedToDeath :: RandomGen g
               => StateT SugAgentState (SugAgentMonadT g) Bool
starvedToDeath = do
  sugar <- gets sugAgSugarLevel
  spice <- gets sugAgSpiceLevel

  if _enableSpice_ 
    then return $ (sugar <= 0) || (spice <= 0)
    else return $ sugar <= 0

agentMove :: RandomGen g
          => AgentId
          -> StateT SugAgentState (SugAgentMonadT g) ()
agentMove aid = do
  cellsInSight <- agentLookout
  coord <- gets sugAgCoord

  let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

  ifThenElse 
    (null unoccupiedCells)
    agentStayAndHarvest
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ cellAtM coord
        let unoccupiedCells' = (coord, selfCell) : unoccupiedCells

        let bf = bestCellFunc
        let bestCells = selectBestCells bf coord unoccupiedCells'
        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bestCells
        agentMoveAndHarvestCell aid cellCoord)

agentLookout :: RandomGen g
             => StateT SugAgentState (SugAgentMonadT g) [(Discrete2dCoord, SugEnvCell)]
agentLookout = do
  vis <- gets sugAgVision
  coord <- gets sugAgCoord
  lift $ lift $ neighboursInNeumannDistanceM coord vis False

agentStayAndHarvest :: RandomGen g
                    => StateT SugAgentState (SugAgentMonadT g) ()
agentStayAndHarvest = gets sugAgCoord >>= agentHarvestCell

agentMoveAndHarvestCell :: RandomGen g
                        => AgentId
                        -> Discrete2dCoord 
                        -> StateT SugAgentState (SugAgentMonadT g) ()
agentMoveAndHarvestCell aid cellCoord = do
  agentHarvestCell cellCoord 
  agentMoveTo aid cellCoord

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> StateT SugAgentState (SugAgentMonadT g) ()
agentMoveTo aid cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  s <- get

  cell <- lift $ lift $ cellAtM cellCoord
  let co = cell { sugEnvOccupier = Just (cellOccupier aid s) }
  lift $ lift $ changeCellAtM cellCoord co 

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> StateT SugAgentState (SugAgentMonadT g) ()
agentHarvestCell cellCoord = do
  cell <- lift $ lift $ cellAtM cellCoord

  sugarLevelAgent <- gets sugAgSugarLevel
  spiceLevelAgent <- gets sugAgSpiceLevel

  let sugarLevelCell = sugEnvSugarLevel cell
  let spiceLevelCell = sugEnvSpiceLevel cell

  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
  let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

  let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
  lift $ lift $ changeCellAtM cellCoord cellHarvested
  
  -- NOTE: at the moment harvesting SPICE does not influence the polution
  lift $ lift $ modify $ poluteCell (sugarLevelCell * polutionHarvestFactor ) cellCoord