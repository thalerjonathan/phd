{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent 
  ( agentSf
  , dieOfAge
  , agentMetabolism
  , agentDies
  , starvedToDeath
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Utils

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
agentSf :: RandomGen g 
        => SugarScapeParams
        -> AgentId
        -> SugAgentState
        -> SugAgent g
agentSf params aid s0 = feedback s0 (proc (evt, s) -> do
  t        <- time -< ()
  let age = floor t
  (ao, s') <- arrM (\(age, evt, s) -> lift $ runStateT (eventMatching evt params aid age) s) -< (age, evt, s)
  returnA -< (ao, s'))

------------------------------------------------------------------------------------------------------------------------
-- Chapter III: Sex, Culture and Conflict: The Emergence of History
------------------------------------------------------------------------------------------------------------------------
eventMatching :: RandomGen g 
              => ABSEvent SugEvent
              -> SugarScapeParams
              -> AgentId
              -> Int
              -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
eventMatching TimeStep params aid age           
  = timeStep params aid age
eventMatching (DomainEvent (sender, MatingRequest)) _ aid _ = do
  ao <- observable
  trace ("Agent " ++ show aid ++ ": incoming MatingRequest from agent " ++ show sender ++ ", will reply with MatingReply!") 
    (return $ sendEventTo sender MatingReply ao)
eventMatching (DomainEvent (sender, MatingReply)) _ aid _
  = trace ("Agent " ++ show aid ++ ": incoming MatingReply from agent " ++ show sender) observable

--error "undefined event in agent, terminating!"
-- eventMatching _ _ _ _                        = error "undefined event in agent, terminating!"

timeStep :: RandomGen g 
         => SugarScapeParams
         -> AgentId
         -> Int
         -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
timeStep params aid age = do
  agentAgeing age
  
  harvestAmount <- agentMove params aid
  metabAmount   <- agentMetabolism

  -- TODO: is this the right order?
  ao <- agentMating params aid

  agentPolute params harvestAmount (fromIntegral metabAmount)

  ao' <- ifThenElseM
          (starvedToDeath `orM` dieOfAge)
          (agentDies params)
          observable

  return $ ao <°> ao'

agentMating :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
agentMating params aid
    | not $ spSexRuleActive params = return agentOut
    | otherwise = ifThenElseM
                    isAgentFertile
                    mateWithNeighbours
                    (return agentOut)
  where
    mateWithNeighbours :: StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
    mateWithNeighbours = do
      coord  <- agentProperty sugAgCoord
      ns     <- lift $ lift $ neighboursM coord False
      let ocs     = filter (siteOccupied . snd) ns
      let _freeNs = filter (siteUnoccupied . snd) ns
      mateWith ocs

    mateWith :: [(Discrete2dCoord, SugEnvSite)] 
             -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
    mateWith [] = return agentOut
    mateWith ((c, s) : ns) = do
      agentState <- get
      let naid = sugEnvOccId $ fromJust $ sugEnvSiteOccupier s
      let cont = matingCont agentState

      return $ 
        trace ("Agent " ++ show aid ++ ": sending MatingRequest to agent " ++ show naid) 
              (sendEventToWithCont naid MatingRequest cont agentOut)

    matingCont :: SugAgentState
               -> SugAgent g
    matingCont s = proc evt -> do
      let ao = agentOutObservable $ sugObservableFromState s 
      returnA -< trace ("Agent " ++ show aid ++ ": holy fuck! We are in the continuation! Received " ++ show evt) ao

isAgentFertile :: MonadState SugAgentState m
               => m Bool
isAgentFertile = isAgentFertileAge `andM` isAgentFertileWealth

isAgentFertileAge :: MonadState SugAgentState m
                  => m Bool
isAgentFertileAge = do
  age        <- agentProperty sugAgAge
  (from, to) <- agentProperty sugAgFertAgeRange
  return $ age >= from && age <= to

isAgentFertileWealth :: MonadState SugAgentState m
                     => m Bool
isAgentFertileWealth = do
  sugLvl     <- agentProperty sugAgSugarLevel
  initSugLvl <- agentProperty sugAgInitSugEndow
  return $ sugLvl >= initSugLvl

agentMove :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> StateT SugAgentState (SugAgentMonadT g) Double
agentMove params aid = do
  cellsInSight <- agentLookout
  coord        <- agentProperty sugAgCoord

  let uoc = filter (siteUnoccupied . snd) cellsInSight

  ifThenElse 
    (null uoc)
    (agentHarvestCell coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ cellAtM coord
        
        let uoc' = (coord, selfCell) : uoc
            bf   = bestSiteFunc params
            bcs  = selectBestSites bf coord uoc'

        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo aid cellCoord
        agentHarvestCell cellCoord)

agentLookout :: RandomGen g
             => StateT SugAgentState (SugAgentMonadT g) [(Discrete2dCoord, SugEnvSite)]
agentLookout = do
  vis   <- agentProperty sugAgVision
  coord <- agentProperty sugAgCoord
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
  let co = cell { sugEnvSiteOccupier = Just (siteOccupier aid s) }
  lift $ lift $ changeCellAtM cellCoord co 

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> StateT SugAgentState (SugAgentMonadT g) Double
agentHarvestCell cellCoord = do
  cell   <- lift $ lift $ cellAtM cellCoord
  sugLvl <- agentProperty sugAgSugarLevel

  let sugLvlSite         = sugEnvSiteSugarLevel cell
      newSugarLevelAgent = sugLvlSite + sugLvl

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvSiteSugarLevel = 0 }
  lift $ lift $ changeCellAtM cellCoord cellHarvested

  return sugLvlSite

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
      let c' = c { sugEnvSitePolutionLevel = sugEnvSitePolutionLevel c + polution }
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
    (newA, newAState)   <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) (agentSf params) id

    -- need to occupy the cell to prevent other agents occupying it
    let newCell' = newCell { sugEnvSiteOccupier = Just (siteOccupier newAid newAState) }
    lift $ lift $ changeCellAtM newCoord newCell' 

    return (newAid, newA)
  where
    -- the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => StateT SugAgentState (SugAgentMonadT g) (Discrete2dCoord, SugEnvSite)
    findUnoccpiedRandomPosition = do
      e          <- lift $ lift get
      (c, coord) <- lift $ lift $ lift $ randomCell e -- TODO: replace by randomCellM
      ifThenElse
        (siteOccupied c) 
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
