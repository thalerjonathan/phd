{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Rebirthing 
  ( agentDies
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

-- this is rule R implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeParams
          -> SugarScapeAgent g
          -> AgentAction g (SugAgentOut g)
agentDies params asf = do
  unoccupyPosition
  ao <- liftM kill agentOutObservableM 
  if spReplaceAgents params
    then do
      (_, newA) <- rebirthNewAgent params asf
      return $ newAgent newA ao
    else return ao

rebirthNewAgent :: RandomGen g
                => SugarScapeParams
                -> SugarScapeAgent g
                -> AgentAction g (AgentId, SugAgentDef g)
rebirthNewAgent params asf = do
    newAid              <- lift nextAgentId
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, _newAState)   <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) asf id

    -- need to occupy the cell to prevent other agents occupying it
    let newCell' = newCell { sugEnvSiteOccupier = Just (siteOccupier newAid) }
    lift $ lift $ changeCellAtM newCoord newCell' 

    return (newAid, newA)
  where
    -- the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => AgentAction g (Discrete2dCoord, SugEnvSite)
    findUnoccpiedRandomPosition = do
      e          <- lift $ lift get
      (c, coord) <- lift $ lift $ lift $ randomCell e -- TODO: replace by randomCellM
      ifThenElse
        (siteOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))