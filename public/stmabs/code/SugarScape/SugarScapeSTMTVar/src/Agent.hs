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
         => Bool
         -> AgentId
         -> SugAgentState
         -> SugAgent g
sugAgent rebirthFlag aid s0 = feedback s0 (proc (ain, s) -> do
  age      <- time -< ()
  (ao, s') <- arrM (\(age, ain, s) -> lift $ runStateT (chapterII rebirthFlag aid ain age) s) -< (age, ain, s)
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
          => Bool
          -> AgentId
          -> SugAgentIn
          -> Time
          -> StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
chapterII rebirthFlag aid _ain _age = do
  ao <- agentMetabolism
  ifThenElse
    (isDead ao)
    (if rebirthFlag
      then do
        (newAid, newA) <- birthNewAgent rebirthFlag
        return $ newAgent newAid newA ao
      else return ao)
    (do
      agentMove aid
      ao' <- observable 
      return $ ao <°> ao')

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
  where
    starvedToDeath :: RandomGen g
                  => StateT SugAgentState (SugAgentMonad g) Bool
    starvedToDeath = do
      sugar <- gets sugAgSugarLevel
      return $ sugar <= 0
      
    agentDies :: RandomGen g
              => StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
    agentDies = do
      env  <- lift readEnvironment
      env' <- unoccupyPosition env
      lift $ writeEnvironment env'

      return $ kill agentOut

birthNewAgent :: RandomGen g
              => Bool
              -> StateT SugAgentState (SugAgentMonad g) (AgentId, SugAgent g)
birthNewAgent rebirthFlag = do
      env                 <- lift readEnvironment
      newAid              <- lift nextAgentId
      (newCoord, newCell) <- findUnoccpiedRandomPosition env
      (newA, newAState)   <- lift $ lift $ randomAgent (newAid, newCoord) (sugAgent rebirthFlag) id

      -- need to occupy the cell to prevent other agents to occupy it before the spawning of the agent
      let newCell' = newCell { sugEnvOccupier = Just (cellOccupier newAid newAState) }
          env'     = changeCellAt newCoord newCell' env

      lift $ writeEnvironment env'

      return (newAid, newA)
  where
    findUnoccpiedRandomPosition :: RandomGen g
                                => SugEnvironment
                                -> StateT SugAgentState (SugAgentMonad g) (Discrete2dCoord, SugEnvCell)
    findUnoccpiedRandomPosition env = do
      let (maxX, maxY) = envDisc2dDims env

      randX <- lift $ lift $ getRandomR (0, maxX - 1) 
      randY <- lift $ lift $ getRandomR (0, maxY - 1)

      let randCoord = (randX, randY)
          c         = cellAt randCoord env

      ifThenElse
        (cellOccupied c) 
        (findUnoccpiedRandomPosition env)
        (return (randCoord, c))

agentMove :: RandomGen g
          => AgentId
          -> StateT SugAgentState (SugAgentMonad g) ()
agentMove aid = do
  env          <- lift readEnvironment
  cellsInSight <- agentLookout env
  coord        <- gets sugAgCoord

  let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

  ifThenElse 
    (null unoccupiedCells)
    (do
      env' <- agentHarvestCell coord env
      lift $ writeEnvironment env')
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        let selfCell         = cellAt coord env
        let unoccupiedCells' = (coord, selfCell) : unoccupiedCells
        let bf               = bestCellFunc
        let bestCells        = selectBestCells bf coord unoccupiedCells'
        (cellCoord, _) <- lift $ lift $ randomElemM bestCells
        env'           <- agentHarvestCell cellCoord env
        env''          <- agentMoveTo aid cellCoord env'
        lift $ writeEnvironment env'')

------------------------------------------------------------------------------
-- ALL FUNCTIONS BELOW TAKE AN ENVIRONMENT AND MAY RETURN AN ENVIRONMENT
agentLookout :: RandomGen g
             => SugEnvironment
             -> StateT SugAgentState (SugAgentMonad g) [(Discrete2dCoord, SugEnvCell)]
agentLookout env = do
  vis   <- gets sugAgVision
  coord <- gets sugAgCoord
  return $ neighboursInNeumannDistance coord vis False env

unoccupyPosition :: RandomGen g
                 => SugEnvironment
                 -> StateT SugAgentState (SugAgentMonad g) SugEnvironment
unoccupyPosition env = do
    (coord, cell) <- agentCellOnCoord
    let cell' = cell { sugEnvOccupier = Nothing }
        env'  = changeCellAt coord cell' env
    return env'
  where
    agentCellOnCoord :: RandomGen g 
                    => StateT SugAgentState (SugAgentMonad g) (Discrete2dCoord, SugEnvCell)
    agentCellOnCoord = do
      coord <- gets sugAgCoord
      let cell = cellAt coord env
      return (coord, cell)

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> SugEnvironment
             -> StateT SugAgentState (SugAgentMonad g) SugEnvironment
agentMoveTo aid cellCoord env = do
  env' <- unoccupyPosition env

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  s   <- get

  let cell = cellAt cellCoord env'
      co   = cell { sugEnvOccupier = Just (cellOccupier aid s) }
      env'' = changeCellAt cellCoord co env'

  return env''

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> SugEnvironment
                 -> StateT SugAgentState (SugAgentMonad g) SugEnvironment
agentHarvestCell cellCoord env = do
  let cell = cellAt cellCoord env

  sugarLevelAgent <- gets sugAgSugarLevel

  let sugarLevelCell     = sugEnvSugarLevel cell
  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvSugarLevel = 0.0 }
      env'          = changeCellAt cellCoord cellHarvested env

  return env'