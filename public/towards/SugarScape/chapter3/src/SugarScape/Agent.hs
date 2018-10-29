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
  t        <- time -< () -- TODO: this will not work when we are switching into new sf => age will start with 0 => no need for SF! we simply resort back to MSFs
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
eventMatching TimeStep params myId age           
  = timeStep params myId age
eventMatching (DomainEvent (sender, MatingRequest otherGender)) _ myId _ 
  = handleMatingRequest myId sender otherGender
eventMatching (DomainEvent (sender, MatingReply accept)) _ myId _
  = handleMatingReply myId sender accept
eventMatching _ _ _ _                        
  = error "undefined event in agent, terminating!"

timeStep :: RandomGen g 
         => SugarScapeParams
         -> AgentId
         -> Int
         -> AgentAction g (SugAgentOut g)
timeStep params myId age = do
  agentAgeing age
  
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