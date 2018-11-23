{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Agent 
  ( agentMsf
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.State
import Control.Monad.Trans.MSF.Reader
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
import SugarScape.Core.Utils

------------------------------------------------------------------------------------------------------------------------
agentMsf :: RandomGen g => SugarScapeAgent g
agentMsf params aid s0 = feedback s0 (proc (evt, s) -> do
  (s', ao) <- runStateS (runReaderS generalEventHandler) -< (s, ((params, aid), evt))
  returnA -< (ao, s'))
  
generalEventHandler :: RandomGen g => EventHandler g
generalEventHandler =
  -- optionally switching the top event handler 
  continueWithAfter 
    (proc evt -> 
      case evt of 
        TickStart dt -> 
          arrM handleTick -< dt

        -- MATING EVENTS
        (DomainEventWithReply sender (MatingRequest otherGender) replyCh receiveCh) -> do
          ao <- arrM (uncurry4 replyMatingRequest) -< (sender, replyCh, receiveCh, otherGender)
          returnA -< (ao, Nothing)
        (Reply sender (MatingTx childId) ch _) -> do
          ao <- arrM (uncurry3 handleMatingTxReply) -< (sender, ch, childId)
          returnA -< (ao, Nothing)

        -- INHERITANCE EVENTS
        (DomainEvent _ (Inherit share)) -> do
          ao <- arrM handleInheritance -< share
          returnA -< (ao, Nothing)

        -- CULTURAL PROCESS EVENTS
        (DomainEvent sender (CulturalProcess tag)) -> do
          ao <- arrM (uncurry handleCulturalProcess) -< (sender, tag)
          returnA -< (ao, Nothing)

        -- COMBAT EVENTS
        (DomainEvent sender KilledInCombat) -> do
          ao <- arrM handleKilledInCombat -< sender
          returnA -< (ao, Nothing)
        
        -- TRADING EVENTS
        (DomainEvent sender (TradingOffer traderMrsBefore traderMrsAfter)) -> do
          ao <- arrM (uncurry3 handleTradingOffer) -< (sender, traderMrsBefore, traderMrsAfter)
          returnA -< (ao, Nothing)

        -- LOAN EVENTS
        (DomainEvent _ (LoanOffer loan)) -> do
          ao <- arrM handleLoanOffer -< loan
          returnA -< (ao, Nothing)
        (DomainEvent sender (LoanPayback loan sugarBack spiceBack)) -> do
          ao <- arrM (uncurry4 handleLoanPayback) -< (sender, loan, sugarBack, spiceBack)
          returnA -< (ao, Nothing)
        (DomainEvent sender (LoanLenderDied children)) -> do
          ao <- arrM (uncurry handleLoanLenderDied) -< (sender, children)
          returnA -< (ao, Nothing)
        (DomainEvent sender (LoanInherit loan)) -> do
          ao <- arrM (uncurry handleLoanInherit) -< (sender, loan)
          returnA -< (ao, Nothing)

        -- DISEASE EVENTS
        (DomainEvent _ (DiseaseTransmit disease)) -> do
          ao <- arrM handleDiseaseTransmit -< disease
          returnA -< (ao, Nothing)
        _        -> do
          aid <- constM myId -< ()
          returnA -< error $ "Agent " ++ show aid ++ ": received unexpected event " ++ show evt ++ " in general handler, terminating!")

handleTick :: RandomGen g 
           => DTime
           -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
handleTick dt = do
  agentAgeing dt
  
  harvestAmount <- agentMove
  metabAmount   <- agentMetabolism
  -- initialize net-income to gathering minus metabolism, might adjusted later during Loan-handling
  updateAgentState (\s -> s { sugAgNetIncome = harvestAmount - fromIntegral metabAmount})
  -- compute polution and diffusion
  agentPolute harvestAmount (fromIntegral metabAmount)

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to apply other rules
  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (do
      aoDie <- agentDies agentMsf
      return (aoDie, Nothing))
    (agentMating agentMsf agentContAfterMating)

agentContAfterMating :: RandomGen g 
                     => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterMating = do
  aoCulture       <- agentCultureProcess 
  (aoTrade, mhdl) <- agentTrade agentContAfterTrading

  let ao = aoCulture `agentOutMergeRight` aoTrade
  return (ao, mhdl)

agentContAfterTrading :: RandomGen g 
                      => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterTrading = do
  (aoLoan, mhdl) <- agentLoan agentContAfterLoan
  return (aoLoan, mhdl)

agentContAfterLoan :: RandomGen g 
                   => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterLoan = do
  (aoDisease, mhdl) <- agentDisease defaultCont
  return (aoDisease, mhdl)

defaultCont :: RandomGen g 
            => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
defaultCont = do
  ao <- agentObservableM
  return (ao, Just generalEventHandler)