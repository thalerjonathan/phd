{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent 
  ( agentMsf
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.State
import Data.MonadicStreamFunction

import SugarScape.Agent.Ageing
import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Mating
import SugarScape.Agent.Metabolism
import SugarScape.Agent.Move
import SugarScape.Agent.Polution
import SugarScape.Agent.Rebirthing
import SugarScape.Agent.Utils
import SugarScape.Model
import SugarScape.Utils

--import Debug.Trace as DBG

------------------------------------------------------------------------------------------------------------------------
agentMsf :: RandomGen g => SugarScapeAgent g
agentMsf params aid s0 = feedback s0 (proc (evt, s) -> do
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
  continueWithAfter 
    (proc evt -> 
      case evt of 
        TimeStep -> 
          arrM_ (handleTimeStep params myId) -< ()
        (DomainEvent (sender, MatingRequest otherGender)) -> do
          ao <- arrM (uncurry (handleMatingRequest myId)) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, MatingTx childId)) -> do
          ao <- arrM (uncurry (handleMatingTx myId)) -< (sender, childId)
          returnA -< (ao, Nothing)
        _        -> 
          returnA -< error $ "Agent " ++ show myId ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTimeStep :: RandomGen g 
               => SugarScapeParams
               -> AgentId
               -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
handleTimeStep params myId = do
  --DBG.trace ("Agent " ++ show myId ++ ": handleTimeStep") 
  agentAgeing
  
  harvestAmount <- agentMove params myId
  metabAmount   <- agentMetabolism
  agentPolute params harvestAmount (fromIntegral metabAmount)

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to mate
  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (do
      ao <- agentDies params agentMsf
      return (ao, Nothing))
    (do 
      ret <- agentMating 
              params 
              myId 
              agentMsf
              (generalEventHandler params myId) 
              (agentContAfterMating params myId)

      case ret of
        Nothing -> do
          ao <- agentContAfterMating params myId
          return (ao, Nothing)
        Just (ao, mhdl) -> return (ao, mhdl))

agentContAfterMating :: RandomGen g 
                     => SugarScapeParams
                     -> AgentId
                     -> AgentAction g (SugAgentOut g)
agentContAfterMating _params _myId = do
  --DBG.trace ("Agent " ++ show myId ++ ": agentContAfterMating") 
  agentOutObservableM