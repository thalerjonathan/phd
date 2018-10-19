{-# LANGUAGE Arrows #-}
module SugarScape.Agent 
  ( sugAgent
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

------------------------------------------------------------------------------------------------------------------------
sugAgent :: RandomGen g 
         => SugarScapeParams
         -> AgentId
         -> SugAgentState
         -> SugAgent g
sugAgent params aid s0 = feedback s0 (proc (ain, s) -> do
  t        <- time -< ()
  let age = floor t
  (ao, s') <- arrM (\(age, ain, s) -> lift $ runStateT (chapterII params aid ain age) s) -< (age, ain, s)
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
          => SugarScapeParams
          -> AgentId
          -> SugAgentIn
          -> Int
          -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
chapterII params aid _ain age = do
  agentAgeing age
  
  harvestAmount <- agentMove params aid
  metabAmount   <- agentMetabolism

  agentPolute params harvestAmount (fromIntegral metabAmount)

  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (agentDies params)
    observable

agentAgeing :: RandomGen g
            => Int
            -> StateT SugAgentState (SugAgentMonadT g) ()
agentAgeing age = updateAgentState (\s' -> s' { sugAgAge = age })

agentMove :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> StateT SugAgentState (SugAgentMonadT g) Double
agentMove params aid = do
  cellsInSight <- agentLookout
  coord        <- gets sugAgCoord

  let uoc = filter (cellUnoccupied . snd) cellsInSight

  ifThenElse 
    (null uoc)
    (agentHarvestCell coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ cellAtM coord
        
        let uoc' = (coord, selfCell) : uoc
            bf   = bestCellFunc params
            bcs  = selectBestCells bf coord uoc'

        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo aid cellCoord
        agentHarvestCell cellCoord)

agentLookout :: RandomGen g
             => StateT SugAgentState (SugAgentMonadT g) [(Discrete2dCoord, SugEnvCell)]
agentLookout = do
  vis   <- gets sugAgVision
  coord <- gets sugAgCoord
  lift $ lift $ neighboursInNeumannDistanceM coord vis False

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> StateT SugAgentState (SugAgentMonadT g) ()
agentMoveTo aid cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  s <- get

  cell <- lift $ lift $ cellAtM cellCoord
  let co = cell { sugEnvCellOccupier = Just (cellOccupier aid s) }
  lift $ lift $ changeCellAtM cellCoord co 

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> StateT SugAgentState (SugAgentMonadT g) Double
agentHarvestCell cellCoord = do
  cell <- lift $ lift $ cellAtM cellCoord

  sugarLevelAgent <- gets sugAgSugarLevel

  let sugarLevelCell     = sugEnvCellSugarLevel cell
  let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvCellSugarLevel = 0 }
  lift $ lift $ changeCellAtM cellCoord cellHarvested

  return sugarLevelCell

agentMetabolism :: RandomGen g
                => StateT SugAgentState (SugAgentMonadT g) Int
agentMetabolism = do
  sugarMetab <- gets sugAgSugarMetab
  sugarLevel <- gets sugAgSugarLevel

  let sugarLevel' = max 0 (sugarLevel - fromIntegral sugarMetab)

  updateAgentState (\s' -> s' { sugAgSugarLevel = sugarLevel' })

  return sugarMetab

agentPolute :: RandomGen g
            => SugarScapeParams
            -> Double
            -> Double
            -> StateT SugAgentState (SugAgentMonadT g) ()
agentPolute params s m = agentPoluteAux $ spPolutionFormation params
  where
    agentPoluteAux :: RandomGen g
                   => PolutionFormation 
                   -> StateT SugAgentState (SugAgentMonadT g) ()
    agentPoluteAux NoPolution = return ()
    agentPoluteAux (Polute a b) = do
      let polution = a * s + b * m

      (coord, c) <- agentCellOnCoord
      let c' = c { sugEnvCellPolutionLevel = sugEnvCellPolutionLevel c + polution }
      lift $ lift $ changeCellAtM coord c'

-- this is rule R implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeParams
          -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
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
              -> StateT SugAgentState (SugAgentMonadT g) (AgentId, SugAgentDef g)
birthNewAgent params = do
    newAid              <- lift nextAgentId
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, newAState)   <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) (sugAgent params) id

    -- need to occupy the cell to prevent other agents occupying it
    let newCell' = newCell { sugEnvCellOccupier = Just (cellOccupier newAid newAState) }
    lift $ lift $ changeCellAtM newCoord newCell' 

    return (newAid, newA)
  where
    -- the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => StateT SugAgentState (SugAgentMonadT g) (Discrete2dCoord, SugEnvCell)
    findUnoccpiedRandomPosition = do
      e          <- lift $ lift get
      (c, coord) <- lift $ lift $ lift $ randomCell e
      ifThenElse
        (cellOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))

dieOfAge :: RandomGen g
          => StateT SugAgentState (SugAgentMonadT g) Bool
dieOfAge = do
  ageSpan <- gets sugAgMaxAge
  case ageSpan of 
    Nothing -> return False
    Just maxAge -> do
      age <- gets sugAgAge
      return $ age >= maxAge

starvedToDeath :: RandomGen g
               => StateT SugAgentState (SugAgentMonadT g) Bool
starvedToDeath = do
  sugar <- gets sugAgSugarLevel
  return $ sugar <= 0

unoccupyPosition :: RandomGen g
                 => StateT SugAgentState (SugAgentMonadT g) ()
unoccupyPosition = do
  (coord, cell) <- agentCellOnCoord
  let cell' = cell { sugEnvCellOccupier = Nothing }
  lift $ lift $ changeCellAtM coord cell'

agentCellOnCoord :: RandomGen g
                => StateT SugAgentState (SugAgentMonadT g) (Discrete2dCoord, SugEnvCell)
agentCellOnCoord = do
  coord <- gets sugAgCoord
  cell  <- lift $ lift $ cellAtM coord
  return (coord, cell)