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

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
agentSf :: RandomGen g => SugarScapeAgent g
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
          (agentDies params agentSf)
          observable

  return $ ao <°> ao'