{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Birthing 
  ( agentDies
  , birthNewAgent
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

-- this is rule R implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeParams
          -> SugarScapeAgent g
          -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
agentDies params asf = do
  unoccupyPosition
  ao <- liftM kill agentOutObservableM 
  if spReplaceAgents params
    then do
      (_, newA) <- birthNewAgent params asf
      return $ newAgent newA ao
    else return ao

birthNewAgent :: RandomGen g
              => SugarScapeParams
              -> SugarScapeAgent g
              -> StateT SugAgentState (SugAgentMonadT g) (AgentId, SugAgentDef g)
birthNewAgent params asf = do
    newAid              <- lift nextAgentId
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, newAState)   <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) asf id

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