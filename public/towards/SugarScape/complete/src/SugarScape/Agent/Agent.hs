{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Agent 
  ( agentMsf
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.State
import Data.MonadicStreamFunction

import SugarScape.Agent.Ageing
import SugarScape.Agent.Common
import SugarScape.Agent.Credit
import SugarScape.Agent.Culture
import SugarScape.Agent.Dying
import SugarScape.Agent.Interface
import SugarScape.Agent.Mating
import SugarScape.Agent.Metabolism
import SugarScape.Agent.Move
import SugarScape.Agent.Polution
import SugarScape.Agent.Trading
import SugarScape.Agent.Utils
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

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
                    => SugarScapeScenario
                    -> AgentId 
                    -> EventHandler g
generalEventHandler params myId =
  -- optionally switching the top event handler 
  continueWithAfter 
    (proc evt -> 
      case evt of 
        TimeStep -> 
          constM (handleTimeStep params myId) -< ()
        (DomainEvent (sender, MatingRequest otherGender)) -> do
          ao <- arrM (uncurry (handleMatingRequest myId)) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, MatingTx childId)) -> do
          ao <- arrM (uncurry (handleMatingTx myId)) -< (sender, childId)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, Inherit share)) -> do
          ao <- arrM (uncurry (handleInheritance myId)) -< (sender, share)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, CulturalProcess tag)) -> do
          ao <- arrM (uncurry (handleCulturalProcess myId)) -< (sender, tag)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, KilledInCombat)) -> do
          ao <- arrM (handleKilledInCombat myId) -< sender
          returnA -< (ao, Nothing)
        (DomainEvent (sender, TradingOffer traderMrsBefore traderMrsAfter)) -> do
          ao <- arrM (uncurry3 (handleTradingOffer myId)) -< (sender, traderMrsBefore, traderMrsAfter)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, CreditOffer sugar spice)) -> do
          ao <- arrM (uncurry3 (handleCreditOffer myId)) -< (sender, sugar, spice)
          returnA -< (ao, Nothing)
        _        -> 
          returnA -< error $ "Agent " ++ show myId ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTimeStep :: RandomGen g 
               => SugarScapeScenario
               -> AgentId
               -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
handleTimeStep params myId = do
  agentAgeing
  
  (harvestAmount, aoMove) <- agentMove params myId
  metabAmount             <- agentMetabolism params myId
  agentPolute params harvestAmount (fromIntegral metabAmount)

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to apply other rules
  ifThenElseM
    (starvedToDeath params `orM` dieOfAge)
    (do
      aoDie <- agentDies params myId agentMsf
      let ao = aoMove `agentOutMergeRightObs` aoDie
      return (ao, Nothing))
    (do 
      let cont = agentContAfterMating params myId

      (aoMating, mhdl) <- agentMating 
                            params 
                            myId 
                            agentMsf
                            cont

      let ao = aoMove `agentOutMergeRightObs` aoMating
      return (ao, mhdl))

agentContAfterMating :: RandomGen g 
                     => SugarScapeScenario
                     -> AgentId
                     -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterMating params myId = do
  aoCulture       <- agentCultureProcess params myId
  (aoTrade, mhdl) <- agentTrade params myId (generalEventHandler params myId)

  -- TODO: do credit here

  let ao = aoCulture `agentOutMergeRightObs` aoTrade
  return (ao, mhdl)