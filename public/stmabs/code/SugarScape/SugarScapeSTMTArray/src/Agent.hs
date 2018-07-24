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
chapterII aid _ain age = do
  ao <- agentAgeing age

  ifThenElse
    (isDead ao)
    (return ao)
    (do
      ao' <- agentMetabolism
      ifThenElse
        (isDead ao')
        (return $ ao <°> ao')
        (do
          agentNonCombatMove aid
          ao'' <- observable 
          return $ ao <°> ao' <°> ao''))

agentAgeing :: RandomGen g 
            => Time 
            -> StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
agentAgeing newAge = do
  updateAgentState (\s -> s { sugAgAge = newAge })

  ifThenElseM
    dieFromAge
    (do
        (aid, a) <- birthNewAgent
        passWealthOn
        ao <- agentDies
        return $ newAgent aid a ao)
    (return agentOut)

dieFromAge :: Monad m
           => StateT SugAgentState m Bool
dieFromAge = do
  age    <- gets sugAgAge
  maxAge <- gets sugAgMaxAge
  return $ age > maxAge

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

birthNewAgent :: RandomGen g
              => StateT SugAgentState (SugAgentMonad g) (AgentId, SugAgent g)
birthNewAgent = do
    -- -| not _enableBirthAgentOnAgeDeath_ = return ()
    -- | otherwise = do
      env           <- lift getEnvironment
      newAgentId    <- lift nextAgentId
      newAgentCoord <- findUnoccpiedRandomPosition env   -- NOTE: why not take the same position?
      (newa, _) <- lift $ lift $ randomAgent (newAgentId, newAgentCoord) sugAgent id
      return (newAgentId, newa)
  where
    findUnoccpiedRandomPosition :: RandomGen g
                                => SugEnvironment
                                -> StateT SugAgentState (SugAgentMonad g) Discrete2dCoord
    findUnoccpiedRandomPosition env = do
      let (maxX, maxY) = envDisc2dDims env

      randX <- lift $ lift $ getRandomR (0, maxX - 1) 
      randY <- lift $ lift $ getRandomR (0, maxY - 1)

      let randCoord = (randX, randY)
      c <- lift $ lift $ lift $ cellAt randCoord env

      ifThenElse
        (cellOccupied c) 
        (findUnoccpiedRandomPosition env)
        (return randCoord)

passWealthOn :: RandomGen g
             => StateT SugAgentState (SugAgentMonad g) ()
passWealthOn
    | _enableInheritance_ = do
      sugarLevel  <- gets sugAgSugarLevel
      childrenIds <- gets sugAgChildren

      let hasChildren = (not . null) childrenIds

      when hasChildren $ do 
        let childrenCount = length childrenIds
        let _childrenSugarShare = sugarLevel / fromIntegral childrenCount
        -- TODO: implement
        -- broadcastMessageM (InheritSugar childrenSugarShare) childrenIds
        return ()
    | otherwise = return ()

agentMetabolism :: RandomGen g
                => StateT SugAgentState (SugAgentMonad g) (SugAgentOut g)
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
  env   <- lift getEnvironment
  lift $ lift $ lift $ poluteCell (sugarMetab * polutionMetabolismFactor) coord env

  ifThenElseM
    starvedToDeath
    agentDies
    (return agentOut)

starvedToDeath :: RandomGen g
               => StateT SugAgentState (SugAgentMonad g) Bool
starvedToDeath = do
  sugar <- gets sugAgSugarLevel
  spice <- gets sugAgSpiceLevel

  if _enableSpice_ 
    then return $ (sugar <= 0) || (spice <= 0)
    else return $ sugar <= 0

agentNonCombatMove :: RandomGen g
                   => AgentId
                   -> StateT SugAgentState (SugAgentMonad g) ()
agentNonCombatMove aid = do
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
  ns    <- lift $ lift $ lift $ neighboursInNeumannDistance coord vis False env
  return ns

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
  spiceLevelAgent <- gets sugAgSpiceLevel

  let sugarLevelCell = sugEnvSugarLevel cell
  let spiceLevelCell = sugEnvSpiceLevel cell

  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
  let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

  let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
      
  lift $ lift $ lift $ changeCellAt cellCoord cellHarvested env
  -- NOTE: at the moment harvesting SPICE does not influence the polution
  lift $ lift $ lift $ poluteCell (sugarLevelCell * polutionHarvestFactor) cellCoord env