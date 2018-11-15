{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Dying 
  ( agentDies
  , handleInheritance
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

-- Here rule R is implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- Also rule I is implemented, see page 67...
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeScenario
          -> AgentId
          -> SugarScapeAgent g
          -> AgentAction g (SugAgentOut g)
agentDies params myId asf = do
  unoccupyPosition
  ao  <- fmap kill agentObservableM
  ao' <- birthNewAgent params asf ao
  inheritance params myId ao'

birthNewAgent :: RandomGen g
              => SugarScapeScenario
              -> SugarScapeAgent g
              -> SugAgentOut g
              -> AgentAction g (SugAgentOut g)
birthNewAgent params asf ao
  | not $ spReplaceAgents params = return ao
  | otherwise = do
    newAid              <- lift nextAgentId
    myTribe             <- agentProperty sugAgTribe
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, newAState)   <- randLift $ randomAgent params (newAid, newCoord) asf 
                              (\as -> case myTribe of
                                        Red  -> changeToRedTribe params as
                                        Blue -> changeToBlueTribe params as)

    -- need to occupy the cell to prevent other agents occupying it
    let occ      = occupier newAid newAState
        newCell' = newCell { sugEnvSiteOccupier = Just occ }
    envLift $ changeCellAtM newCoord newCell' 

    return $ newAgent newA ao
  where
    -- TODO: the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => AgentAction g (Discrete2dCoord, SugEnvSite)
    findUnoccpiedRandomPosition = do
      e          <- envLift get
      (c, coord) <- randLift $ randomCell e -- TODO: replace by randomCellM
      ifThenElse
        (siteOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))

inheritance :: RandomGen g
            => SugarScapeScenario
            -> AgentId
            -> SugAgentOut g
            -> AgentAction g (SugAgentOut g)
inheritance params _myId ao
  | not $ spInheritance params = return ao
  | otherwise = do
    sugLvl   <- agentProperty sugAgSugarLevel
    children <- agentProperty sugAgChildren

    -- TODO: send CreditInherit to all borrowers 

    -- only inherit in case 
    -- 1. there is sugar left (performance optimisation) (sugLvl is 0 in case the agent starved to death=
    -- 2. there are actually children
    if sugLvl > 0 && not (null children)
      then do
        let share = sugLvl / fromIntegral (length children)
        return $ broadcastEvent children (Inherit share) ao
      else return ao

handleInheritance :: RandomGen g
                  => AgentId
                  -> AgentId
                  -> Double
                  -> AgentAction g (SugAgentOut g)
handleInheritance myId _parent share = do
  updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + share })
  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteWithOccupier myId
  agentObservableM