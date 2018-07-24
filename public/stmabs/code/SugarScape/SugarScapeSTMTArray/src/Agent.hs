{-# LANGUAGE Arrows #-}
module Agent 
  (
    sugAgent
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

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
                 -> StateT SugAgentState (SugAgentMonad g) ()
updateAgentState = modify

observable :: RandomGen g
           => StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
observable 
  = get >>= \s -> return $ agentOutObservable $ sugObservableFromState s 

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
chapterII :: RandomGen g 
          => AgentId
          -> SugAgentIn
          -> Time
          -> StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
chapterII aid _ain _age = do
  ao <- agentMetabolism
  ifThenElse
    (isDead ao)
    (return ao)
    (do
      agentMove aid
      ao' <- observable 
      return $ ao <°> ao')

agentDies :: RandomGen g
          => StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
agentDies = do
  unoccupyPosition
  return $ kill agentOut

unoccupyPosition :: RandomGen g
                 => StateT SugAgentState (SugAgentMonad g) ()
unoccupyPosition = do
  env           <- lift getEnvironment
  (coord, c) <- agentCellOnCoord env
  let c' = c { sugEnvOccupier = Nothing }
  lift $ lift $ lift $ changeCellAt coord c' env

agentCellOnCoord :: RandomGen g 
                 => SugEnvironment
                 -> StateT SugAgentState (SugAgentMonad g) (Discrete2dCoord, SugEnvCell)
agentCellOnCoord env = do
  coord <- gets sugAgCoord
  cell  <- lift $ lift $ lift $ cellAt coord env
  return (coord, cell)

agentMetabolism :: RandomGen g
                => StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
agentMetabolism = do
  sugarMetab <- gets sugAgSugarMetab
  sugarLevel <- gets sugAgSugarLevel

  let newSugarLevel = max 0 (sugarLevel - sugarMetab)
 
  updateAgentState (\s' -> s' { sugAgSugarLevel = newSugarLevel })

  ifThenElseM
    starvedToDeath
    agentDies
    (return agentOut)

starvedToDeath :: RandomGen g
               => StateT SugAgentState (SugAgentMonad g) Bool
starvedToDeath = do
  sugar <- gets sugAgSugarLevel
  return $ sugar <= 0

agentMove :: RandomGen g
          => AgentId
          -> StateT SugAgentState (SugAgentMonad g) ()
agentMove aid = do
  cellsInSight <- agentLookout
  coord        <- gets sugAgCoord
  env          <- lift getEnvironment

  let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

  ifThenElse 
    (null unoccupiedCells)
    agentStayAndHarvest
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ lift $ cellAt coord env

        let unoccupiedCells' = (coord, selfCell) : unoccupiedCells
        let bf               = bestCellFunc
        let bestCells        = selectBestCells bf coord unoccupiedCells'
        (cellCoord, _) <- lift $ lift $ randomElemM bestCells
        agentMoveAndHarvestCell aid cellCoord)

agentLookout :: RandomGen g
             => StateT SugAgentState (SugAgentMonad g) [(Discrete2dCoord, SugEnvCell)]
agentLookout = do
  vis   <- gets sugAgVision
  coord <- gets sugAgCoord
  env   <- lift getEnvironment
  lift $ lift $ lift $ neighboursInNeumannDistance coord vis False env

agentStayAndHarvest :: RandomGen g
                    => StateT SugAgentState (SugAgentMonad g) ()
agentStayAndHarvest = gets sugAgCoord >>= agentHarvestCell

agentMoveAndHarvestCell :: RandomGen g
                        => AgentId
                        -> Discrete2dCoord 
                        -> StateT SugAgentState (SugAgentMonad g) ()
agentMoveAndHarvestCell aid cellCoord = do
  agentHarvestCell cellCoord 
  agentMoveTo aid cellCoord

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> StateT SugAgentState (SugAgentMonad g) ()
agentMoveTo aid cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  s   <- get
  env <- lift getEnvironment

  c <- lift $ lift $ lift $ cellAt cellCoord env
  let c' = c { sugEnvOccupier = Just (cellOccupier aid s) }
  lift $ lift $ lift $ changeCellAt cellCoord c' env

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> StateT SugAgentState (SugAgentMonad g) ()
agentHarvestCell cellCoord = do
  env  <- lift getEnvironment
  cell <- lift $ lift $ lift $ cellAt cellCoord env

  sugarLevelAgent <- gets sugAgSugarLevel

  let sugarLevelCell = sugEnvSugarLevel cell

  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvSugarLevel = 0.0 }
      
  lift $ lift $ lift $ changeCellAt cellCoord cellHarvested env