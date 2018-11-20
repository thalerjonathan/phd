{-# LANGUAGE Strict #-}
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
import SugarScape.Agent.Culture
import SugarScape.Agent.Disease
import SugarScape.Agent.Dying
import SugarScape.Agent.Interface
import SugarScape.Agent.Loan
import SugarScape.Agent.Mating
import SugarScape.Agent.Metabolism
import SugarScape.Agent.Move
import SugarScape.Agent.Polution
import SugarScape.Agent.Trading
import SugarScape.Agent.Utils
import SugarScape.Core.Common
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

------------------------------------------------------------------------------------------------------------------------
-- TODO: maybe add a reader for AgentId and SugarScapeScenario?
agentMsf :: RandomGen g => SugarScapeAgent g
agentMsf params aid s0 = feedback s0 (proc (evt, s) -> do
  (s', ao) <- runStateS (generalEventHandler params aid) -< (s, evt)
  returnA -< (ao, s'))

generalEventHandler :: RandomGen g 
                    => SugarScapeScenario
                    -> AgentId 
                    -> EventHandler g
generalEventHandler params myId =
  -- optionally switching the top event handler 
  continueWithAfter 
    (proc evt -> 
      case evt of 
        Tick -> 
          constM (handleTick params myId) -< ()
        (DomainEvent (sender, MatingRequest otherGender)) -> do
          ao <- arrM (uncurry (handleMatingRequest myId)) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, MatingTx childId)) -> do
          ao <- arrM (uncurry (handleMatingTx myId)) -< (sender, childId)
          returnA -< (ao, Nothing)
        (DomainEvent (_, Inherit share)) -> do
          ao <- arrM (handleInheritance myId) -< share
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
        (DomainEvent (_, LoanOffer loan)) -> do
          ao <- arrM (handleLoanOffer myId) -< loan
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanPayback loan sugarBack spiceBack)) -> do
          ao <- arrM (uncurry4 (handleLoanPayback params myId)) -< (sender, loan, sugarBack, spiceBack)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanLenderDied children)) -> do
          ao <- arrM (uncurry (handleLoanLenderDied myId)) -< (sender, children)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanInherit loan)) -> do
          ao <- arrM (uncurry (handleLoanInherit myId)) -< (sender, loan)
          returnA -< (ao, Nothing)
        (DomainEvent (_, DiseaseTransmit disease)) -> do
          ao <- arrM handleDiseaseTransmit -< disease
          returnA -< (ao, Nothing)
        _        -> 
          returnA -< error $ "Agent " ++ show myId ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTick :: RandomGen g 
           => SugarScapeScenario
           -> AgentId
           -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
handleTick params myId = do
  agentAgeing
  
  (harvestAmount, aoMove) <- agentMove params myId
  metabAmount             <- agentMetabolism params myId
  -- initialize net-income to gathering minus metabolism, might adjusted later during Loan-handling
  updateAgentState (\s -> s { sugAgNetIncome = harvestAmount - fromIntegral metabAmount})
  -- compute polution and diffusion
  agentPolute params harvestAmount (fromIntegral metabAmount)

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to apply other rules
  ifThenElseM
    (starvedToDeath params `orM` dieOfAge)
    (do
      aoDie <- agentDies params agentMsf
      let ao = aoMove `agentOutMergeRightObs` aoDie
      return (ao, Nothing))
    (do 
      let cont = agentContAfterMating params myId
      
      (aoMating, mhdl) <- agentMating params myId agentMsf cont

      let ao = aoMove `agentOutMergeRightObs` aoMating
      return (ao, mhdl))

agentContAfterMating :: RandomGen g 
                     => SugarScapeScenario
                     -> AgentId
                     -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterMating params myId = do
    aoCulture       <- agentCultureProcess params myId
    (aoTrade, mhdl) <- agentTrade params myId cont

    let ao = aoCulture `agentOutMergeRightObs` aoTrade
    return (ao, mhdl)
  where
    cont = agentContAfterTrading params myId

agentContAfterTrading :: RandomGen g 
                      => SugarScapeScenario
                      -> AgentId
                      -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterTrading params myId = do
  (aoLoan, mhdl) <- agentLoan params myId (agentContAfterLoan params myId)
  return (aoLoan, mhdl)

agentContAfterLoan :: RandomGen g 
                   => SugarScapeScenario
                   -> AgentId
                   -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterLoan params myId = do
    (aoDisease, mhdl) <- agentDisease params cont
    return (aoDisease, mhdl)
  where
    cont = defaultCont params myId

defaultCont :: RandomGen g 
            => SugarScapeScenario
            -> AgentId
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
defaultCont params myId = do
  ao <- agentObservableM
  return (ao, Just $ generalEventHandler params myId)