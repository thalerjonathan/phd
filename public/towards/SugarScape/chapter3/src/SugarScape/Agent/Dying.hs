{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Dying 
  ( agentDies
  , handleInheritance
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

--import Debug.Trace

-- Here rule R is implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- Also rule I is implemented, see page 67...
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> SugarScapeAgent g
          -> AgentAction g (SugAgentOut g)
agentDies params myId asf = do
  unoccupyPosition
  ao  <- liftM kill agentOutObservableM
  ao' <- birthNewAgent params asf ao
  inheritance params myId ao'

birthNewAgent :: RandomGen g
              => SugarScapeParams
              -> SugarScapeAgent g
              -> SugAgentOut g
              -> AgentAction g (SugAgentOut g)
birthNewAgent params asf ao
  | not $ spReplaceAgents params = return ao
  | otherwise = do
    newAid              <- lift nextAgentId
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, _newAState)  <- lift $ lift $ lift $ randomAgent params (newAid, newCoord) asf id

    -- need to occupy the cell to prevent other agents occupying it
    let newCell' = newCell { sugEnvSiteOccupier = Just (siteOccupier newAid) }
    lift $ lift $ changeCellAtM newCoord newCell' 

    return $ newAgent newA ao
  where
    -- TODO: the more cells occupied the less likely an unoccupied position will be found
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

inheritance :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> SugAgentOut g
            -> AgentAction g (SugAgentOut g)
inheritance params _myId ao
  | not $ spInheritance params = agentOutObservableM
  | otherwise = do
    sugLvl   <- agentProperty sugAgSugarLevel
    children <- agentProperty sugAgChildren

    -- only inherit in case 
    -- 1. there is sugar left (performance optimisation) (sugLvl is 0 in case the agent starved to death=
    -- 2. there are actually children
    if sugLvl > 0 && not (null children)
      then do
        let share = sugLvl / fromIntegral (length children)
        return $ 
          --trace ("Agent: " ++ show myId ++ " inheriting " ++ show share ++ " to children " ++ show children) 
          (broadcastEvent children (Inherit share) ao)
      else return ao

handleInheritance :: RandomGen g
                  => AgentId
                  -> AgentId
                  -> Double
                  -> AgentAction g (SugAgentOut g)
handleInheritance _myId _parent share = do
  updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + share })
  --trace ("Agent: " ++ show myId ++ " inheriting " ++ show share ++ " from parent " ++ show parent) 
  agentOutObservableM