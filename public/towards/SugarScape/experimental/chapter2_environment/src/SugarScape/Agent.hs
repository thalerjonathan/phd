{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent 
  ( agentSF
  , dieOfAge
  , agentMetabolism
  , agentDies
  , starvedToDeath
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Utils

type InternalAgentMonadT g = StateT SugEnvironment (AgentT (Rand g))

------------------------------------------------------------------------------------------------------------------------
agentSF :: RandomGen g 
        => SugarScapeParams
        -> AgentId
        -> SugAgentState
        -> SugAgent g
agentSF params aid s0 = feedback s0 (proc (env, s) -> do
  t        <- time -< ()
  let age = floor t
  (ao, s', env') <- arrM (\(age, s, env) -> do
    ((ao, s'), env') <- lift $ runStateT (runStateT (agentBehaviour params aid age) s) env
    return (ao, s', env')) -< (age, s, env)
     
  returnA -< ((ao, env'), s'))

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentBehaviour :: RandomGen g 
               => SugarScapeParams
               -> AgentId
               -> Int
               -> StateT SugAgentState (InternalAgentMonadT g) (SugAgentOut g)
agentBehaviour params aid age = do
  agentAgeing age
  
  harvestAmount <- agentMove params aid
  metabAmount   <- agentMetabolism

  agentPolute params harvestAmount (fromIntegral metabAmount)

  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (agentDies params)
    observable

agentMove :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> StateT SugAgentState (InternalAgentMonadT g) Double
agentMove params aid = do
  cellsInSight <- agentLookout
  coord        <- agentProperty sugAgCoord

  let uoc = filter (cellUnoccupied . snd) cellsInSight

  ifThenElse 
    (null uoc)
    (agentHarvestCell coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ cellAtM coord
        
        let uoc' = (coord, selfCell) : uoc
            bf   = bestCellFunc params
            bcs  = selectBestCells bf coord uoc'

        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo aid cellCoord
        agentHarvestCell cellCoord)

agentLookout :: RandomGen g
             => StateT SugAgentState (InternalAgentMonadT g) [(Discrete2dCoord, SugEnvCell)]
agentLookout = do
  vis   <- agentProperty sugAgVision
  coord <- agentProperty sugAgCoord
  lift $ neighboursInNeumannDistanceM coord vis False

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> StateT SugAgentState (InternalAgentMonadT g) ()
agentMoveTo aid cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  cell <- lift $ cellAtM cellCoord
  let co = cell { sugEnvCellOccupier = Just aid }
  lift $ changeCellAtM cellCoord co 

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> StateT SugAgentState (InternalAgentMonadT g) Double
agentHarvestCell cellCoord = do
  cell <- lift $ cellAtM cellCoord

  sugarLevelAgent <- agentProperty sugAgSugarLevel

  let sugarLevelCell     = sugEnvCellSugarLevel cell
  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvCellSugarLevel = 0 }
  lift $ changeCellAtM cellCoord cellHarvested

  return sugarLevelCell

agentMetabolism :: MonadState SugAgentState m
                => m Int
agentMetabolism = do
  sugarMetab <- agentProperty sugAgSugarMetab
  sugarLevel <- agentProperty sugAgSugarLevel

  let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)

  updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })

  return sugarMetab

agentPolute :: RandomGen g
            => SugarScapeParams
            -> Double
            -> Double
            -> StateT SugAgentState (InternalAgentMonadT g) ()
agentPolute params s m = agentPoluteAux $ spPolutionFormation params
  where
    agentPoluteAux :: RandomGen g
                   => PolutionFormation 
                   -> StateT SugAgentState (InternalAgentMonadT g) ()
    agentPoluteAux NoPolution = return ()
    agentPoluteAux (Polute a b) = do
      let polution = a * s + b * m

      (coord, c) <- agentCellOnCoord
      let c' = c { sugEnvCellPolutionLevel = sugEnvCellPolutionLevel c + polution }
      lift $ changeCellAtM coord c'

-- this is rule R implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeParams
          -> StateT SugAgentState (InternalAgentMonadT g) (SugAgentOut g)
agentDies params = do
  unoccupyPosition
  let ao = kill agentOut
  if spReplaceAgents params
    then do
      (_, newA) <- birthNewAgent params
      return $ newAgent newA ao
    else return ao

birthNewAgent :: RandomGen g
              => SugarScapeParams
              -> StateT SugAgentState (InternalAgentMonadT g) (AgentId, SugAgentDef g)
birthNewAgent params = do
    newAid              <- lift $ lift nextAgentId
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, _)           <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) (agentSF params) id

    -- need to occupy the cell to prevent other agents occupying it
    let newCell' = newCell { sugEnvCellOccupier = Just newAid }
    lift $ changeCellAtM newCoord newCell' 

    return (newAid, newA)
  where
    -- the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => StateT SugAgentState (InternalAgentMonadT g) (Discrete2dCoord, SugEnvCell)
    findUnoccpiedRandomPosition = do
      e          <- lift get
      (c, coord) <- lift $ lift $ lift $ randomCell e -- TODO: replace by randomCellM
      ifThenElse
        (cellOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))

agentAgeing :: MonadState SugAgentState m
            => Int
            -> m ()
agentAgeing age = updateAgentState (\s' -> s' { sugAgAge = age })

dieOfAge :: MonadState SugAgentState m
          => m Bool
dieOfAge = do
  ageSpan <- agentProperty sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- agentProperty sugAgAge
      return $ age >= maxAge

starvedToDeath :: MonadState SugAgentState m
               => m Bool
starvedToDeath = do
  sugar <- agentProperty sugAgSugarLevel
  return $ sugar <= 0

unoccupyPosition :: RandomGen g
                 => StateT SugAgentState (InternalAgentMonadT g) ()
unoccupyPosition = do
  (coord, cell) <- agentCellOnCoord
  let cell' = cell { sugEnvCellOccupier = Nothing }
  lift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                => StateT SugAgentState (InternalAgentMonadT g) (Discrete2dCoord, SugEnvCell)
agentCellOnCoord = do
  coord <- agentProperty sugAgCoord
  cell  <- lift $ cellAtM coord
  return (coord, cell)

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
