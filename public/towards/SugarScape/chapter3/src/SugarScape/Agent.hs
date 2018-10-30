{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent 
  ( agentSf
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.State
import Data.MonadicStreamFunction

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

-- import Debug.Trace as DBG

------------------------------------------------------------------------------------------------------------------------
agentSf :: RandomGen g => SugarScapeAgent g
agentSf params aid s0 = feedback s0 (proc (evt, s) -> do
  (s', ao) <- runStateS (generalEventHandler params aid) -< (s, evt)
  returnA -< (ao, s'))

-- SugAgentMonad g  = StateT SugEnvironment (Rand g)
-- SugAgentMonadT g = StateT ABSState (StateT SugEnvironment (Rand g))
-- AgentMSF m e o =  MSF (AgentT m) (ABSEvent e) (AgentOut m e o)
-- SugAgentMSF g  = AgentMSF (SugAgentMonad g) SugEvent SugAgentObservable
-- MSF (AgentT m) (ABSEvent e) (AgentOut m e o)

generalEventHandler :: RandomGen g 
                    => SugarScapeParams
                    -> AgentId 
                    -> EventHandler g
generalEventHandler params myId =
  -- switching the top event handler to a new one
  -- TODO: need to delay the switching, bcs continuation will be evaluated at time of
  -- switching which would override the old output
  switch 
    (proc evt -> 
      case evt of 
        TimeStep -> 
          arrM_ (handleTimeStep params myId) -< ()
        (DomainEvent (sender, MatingRequest otherGender)) -> do
          ao <- arrM (uncurry (handleMatingRequest myId)) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        _        -> 
        -- NOTE: MatingReply is NOT handled in this continuation!!! See Mathing.hs
          returnA -< error "undefined event in agent, terminating!")
    id
    --(DBG.trace $ "Agent " ++ show myId ++ ": switching in generalEventHandler")

handleTimeStep :: RandomGen g 
               => SugarScapeParams
               -> AgentId
               -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
handleTimeStep params myId = do
  agentAgeing
  
  harvestAmount <- agentMove params myId
  metabAmount   <- agentMetabolism

  ret <- agentMating 
          params 
          myId 
          (generalEventHandler params myId) 
          (agentFinalize params harvestAmount metabAmount)
  case ret of
    Nothing -> do
      ao <- agentFinalize params harvestAmount metabAmount
      return (ao, Nothing)
    Just (ao, mhdl) -> return (ao, mhdl)

agentFinalize :: RandomGen g 
              => SugarScapeParams
              -> Double
              -> Int
              -> AgentAction g (SugAgentOut g)
agentFinalize params harvestAmount metabAmount = do
  agentPolute params harvestAmount (fromIntegral metabAmount)

  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (agentDies params agentSf)
    agentOutObservableM


{-
switchTest :: RandomGen g 
           => SugarScapeParams
           -> AgentId 
           -> MSF (StateT SugAgentState (SugAgentMonadT g)) (ABSEvent SugEvent) (SugAgentOut g)
switchTest params myId = proc evt -> 
  switch 
    (proc evt -> do
      ao <- arrM_ agentOutObservableM -< ()
      DBG.trace "switching now in switchTest" returnA -< (ao, Just ()))
    (const (switchTest' params myId)) -< evt

switchTest' :: RandomGen g 
            => SugarScapeParams
            -> AgentId 
            -> MSF (StateT SugAgentState (SugAgentMonadT g)) (ABSEvent SugEvent) (SugAgentOut g)
switchTest' params myId = proc evt -> 
  switch 
    (proc evt -> do
      ao <- arrM_ agentOutObservableM -< ()
      DBG.trace "switching now in switchTest'" returnA -< (ao, Just ()))
    (const (switchTest params myId)) -< evt
    -}