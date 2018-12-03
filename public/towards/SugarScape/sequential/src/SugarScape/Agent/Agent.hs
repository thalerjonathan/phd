{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Agent 
  ( agentMsf
  ) where

import Control.Monad.Random
import Control.Monad.Trans.MSF.Reader
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
        Tick dt -> 
          arrM handleTick -< dt

        -- MATING EVENTS
        (DomainEvent sender (MatingRequest otherGender)) -> do
          ao <- arrM (uncurry handleMatingRequest) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        (DomainEvent sender (MatingTx childId)) -> do
          ao <- arrM (uncurry handleMatingTx) -< (sender, childId)
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
          returnA -< error $ "Agent " ++ show aid ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTick :: RandomGen g 
           => DTime
           -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
handleTick dt = do
  agentAgeing dt
  
  (harvestAmount, aoMove) <- agentMove
  metabAmount             <- agentMetabolism
  -- initialize net-income to gathering minus metabolism, might adjusted later during Loan-handling
  updateAgentState (\s -> s { sugAgNetIncome = harvestAmount - metabAmount})
  -- compute polution and diffusion
  agentPolute harvestAmount metabAmount

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to apply other rules
  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (do
      aoDie <- agentDies agentMsf

      let ao = aoMove `agentOutMergeRightObs` aoDie
      return (ao, Nothing))
    (do 
      (aoMating, mhdl) <- agentMating agentMsf agentContAfterMating
      
      let ao = aoMove `agentOutMergeRightObs` aoMating
      return (ao, mhdl))

agentContAfterMating :: RandomGen g 
                     => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterMating = do
    aoCulture       <- agentCultureProcess
    (aoTrade, mhdl) <- agentTrade cont

    let ao = aoCulture `agentOutMergeRightObs` aoTrade
    return (ao, mhdl)
  where
    cont = agentContAfterTrading

agentContAfterTrading :: RandomGen g 
                      => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterTrading = do
  (aoLoan, mhdl) <- agentLoan agentContAfterLoan
  return (aoLoan, mhdl)

agentContAfterLoan :: RandomGen g 
                   => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterLoan = do
    (aoDisease, mhdl) <- agentDisease cont
    return (aoDisease, mhdl)
  where
    cont = defaultCont

defaultCont :: RandomGen g 
            => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
defaultCont = do
  ao <- agentObservableM
  return (ao, Just generalEventHandler)