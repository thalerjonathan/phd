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
import FRP.BearRiver

import SugarScape.Agent.Ageing
import SugarScape.Agent.Birthing
import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Mating
import SugarScape.Agent.Metabolism
import SugarScape.Agent.Move
import SugarScape.Agent.Polution
import SugarScape.Model
import SugarScape.Utils

------------------------------------------------------------------------------------------------------------------------
agentSf :: RandomGen g => SugarScapeAgent g
agentSf params aid s0 = feedback s0 (proc (evt, s) -> do
  (ao, s') <- arrM (\(evt, s) -> runStateT (eventMatching evt params aid) s) -< (evt, s)
  returnA -< (ao, s'))

------------------------------------------------------------------------------------------------------------------------
-- Chapter III: Sex, Culture and Conflict: The Emergence of History
------------------------------------------------------------------------------------------------------------------------
eventMatching :: RandomGen g 
              => ABSEvent SugEvent
              -> SugarScapeParams
              -> AgentId
              -> AgentAction g (SugAgentOut g)
eventMatching TimeStep params myId           
  = timeStep params myId
eventMatching (DomainEvent (sender, MatingRequest otherGender)) _ myId
  = handleMatingRequest myId sender otherGender
-- NOTE: this is NOT handled in this continuation!!! See Mathing.hs
-- eventMatching (DomainEvent (sender, MatingReply accept)) _ myId
--  = handleMatingReply myId sender accept
eventMatching _ _ _                
  = error "undefined event in agent, terminating!"

timeStep :: RandomGen g 
         => SugarScapeParams
         -> AgentId
         -> AgentAction g (SugAgentOut g)
timeStep params myId = do
  agentAgeing
  
  harvestAmount <- agentMove params myId
  metabAmount   <- agentMetabolism

  mao <- agentMating params myId (finalize params harvestAmount metabAmount)
  case mao of
    Nothing -> finalize params harvestAmount metabAmount
    Just ao -> return ao

finalize :: RandomGen g 
         => SugarScapeParams
         -> Double
         -> Int
         -> AgentAction g (SugAgentOut g)
finalize params harvestAmount metabAmount = do
  agentPolute params harvestAmount (fromIntegral metabAmount)

  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (agentDies params agentSf)
    agentOutObservableM