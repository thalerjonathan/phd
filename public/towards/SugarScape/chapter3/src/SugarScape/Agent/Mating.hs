{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating
  , handleMatingRequest
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import FRP.BearRiver

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

import Debug.Trace

agentMating :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> AgentAction g (SugAgentOut g)
            -> StateT SugAgentState (SugAgentMonadT g) (Maybe (SugAgentOut g))
agentMating params myId act0
    | not $ spSexRuleActive params = return Nothing
    | otherwise                    = mateWithNeighbours act0
  where
    mateWithNeighbours :: RandomGen g
                       => AgentAction g (SugAgentOut g)
                       -> AgentAction g (Maybe (SugAgentOut g))
    mateWithNeighbours act = do
      coord  <- agentProperty sugAgCoord
      ns     <- lift $ lift $ neighboursM coord False
      if null ns
        then return Nothing
        else do
          let ocs     = filter (siteOccupied . snd) ns
          let _freeNs = filter (siteUnoccupied . snd) ns
          ao <- mateWith myId act ocs
          return $ Just ao
         
handleMatingRequest :: (RandomGen g, MonadState SugAgentState m)
                    => AgentId
                    -> AgentId
                    -> AgentGender
                    -> m (SugAgentOut g)
handleMatingRequest myId sender otherGender = do
  accept <- acceptMatingRequest otherGender
  ao     <- agentOutObservableM
  trace ("Agent " ++ show myId ++ 
         ": incoming (MatingRequest " ++ show otherGender ++ 
         ") from agent " ++ show sender ++ 
         ", will reply with MatingReply " ++ show accept ++ "!") 
    (return $ sendEventTo sender (MatingReply accept) ao)

mateWith :: RandomGen g
         => AgentId
         -> AgentAction g (SugAgentOut g)
         -> [(Discrete2dCoord, SugEnvSite)]
         -> AgentAction g (SugAgentOut g)
mateWith myId cont0 []  = cont0 -- iteration finished, pick up agent-behaviour where it left before starting mating, 
                              -- TODO: need to switch back into agentSf
mateWith myId cont0 ((coord, site) : ns) =
    ifThenElseM
      isAgentFertile
      (do
        -- TODO: check if there is a free site: 
        agentState <- get
        let naid = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site -- fromJust guaranteed not to fail, neighbours contain only occupied sites 
        let cont = matingCont cont0 agentState
        gender <- agentProperty sugAgGender
        ao     <- agentOutObservableM

        return $ 
          trace ("Agent " ++ show myId ++ ": sending (MatingRequest " ++ show gender ++ ") to agent " ++ show naid) 
                (sendEventToWithCont naid (MatingRequest gender) cont ao))
      cont0 -- not fertile, iteration finished, pick up agent-behaviour where it left before starting mating, 
                  -- TODO: need to switch back into agentSf
  where
    matingCont :: RandomGen g
               => AgentAction g (SugAgentOut g)
               -> SugAgentState
               -> SugAgentMSF g
    matingCont cont' s = proc evt -> 
        case evt of
          (DomainEvent (sender, MatingReply accept)) -> do
            (ao, s') <- arrM (\(sender, accept) -> runStateT (handleMatingReply cont' sender accept) s) -< (sender, accept)
            returnA -< ao
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Mating, terminating simulation!"
      where
        handleMatingReply :: RandomGen g
                          => AgentAction g (SugAgentOut g)
                          -> AgentId
                          -> Bool
                          -> AgentAction g (SugAgentOut g)
        handleMatingReply cont'' sender accept 
          -- the sender accepts the mating-request
          | accept = do
            -- 1. calculate new-born genes
            -- 2. create a new born and endow 
            -- 3. continue with next neighbour
            agentOutObservableM 
          -- the sender refusese the mating-request
          | otherwise  = do
            -- continue with next neighbour
            mateWith myId cont'' ns
            -- trace ("Agent " ++ show myId ++ ": incoming (MatingReply " ++ show accept ++ ") from agent " ++ show sender) agentOutObservableM

acceptMatingRequest :: MonadState SugAgentState m
                    => AgentGender
                    -> m Bool
acceptMatingRequest otherGender = do
  myGender <- agentProperty sugAgGender
  fertile  <- isAgentFertile
  return $ (myGender /= otherGender) && fertile 

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