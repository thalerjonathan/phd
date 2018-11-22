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
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

------------------------------------------------------------------------------------------------------------------------
agentMsf :: RandomGen g => SugarScapeAgent g
agentMsf params aid s0 = feedback s0 (proc (evt, s) -> do
  (s', ao) <- runStateS (runReaderS (generalEventHandler params)) -< (s, ((params, aid), evt))
  returnA -< (ao, s'))
  
generalEventHandler :: RandomGen g 
                    => SugarScapeScenario
                    -> EventHandler g
generalEventHandler params =
  -- optionally switching the top event handler 
  continueWithAfter 
    (proc evt -> 
      case evt of 
        TickStart dt -> 
          arrM (handleTick params) -< dt
        (DomainEvent (sender, MatingRequest otherGender)) -> do
          ao <- arrM (uncurry handleMatingRequest) -< (sender, otherGender)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, MatingTx childId)) -> do
          ao <- arrM (uncurry handleMatingTx) -< (sender, childId)
          returnA -< (ao, Nothing)
        (DomainEvent (_, Inherit share)) -> do
          ao <- arrM handleInheritance -< share
          returnA -< (ao, Nothing)
        (DomainEvent (sender, CulturalProcess tag)) -> do
          ao <- arrM (uncurry handleCulturalProcess) -< (sender, tag)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, KilledInCombat)) -> do
          ao <- arrM handleKilledInCombat -< sender
          returnA -< (ao, Nothing)
        (DomainEvent (sender, TradingOffer traderMrsBefore traderMrsAfter)) -> do
          ao <- arrM (uncurry3 handleTradingOffer) -< (sender, traderMrsBefore, traderMrsAfter)
          returnA -< (ao, Nothing)
        (DomainEvent (_, LoanOffer loan)) -> do
          ao <- arrM handleLoanOffer -< loan
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanPayback loan sugarBack spiceBack)) -> do
          ao <- arrM (uncurry4 (handleLoanPayback params)) -< (sender, loan, sugarBack, spiceBack)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanLenderDied children)) -> do
          ao <- arrM (uncurry handleLoanLenderDied) -< (sender, children)
          returnA -< (ao, Nothing)
        (DomainEvent (sender, LoanInherit loan)) -> do
          ao <- arrM (uncurry handleLoanInherit) -< (sender, loan)
          returnA -< (ao, Nothing)
        (DomainEvent (_, DiseaseTransmit disease)) -> do
          ao <- arrM handleDiseaseTransmit -< disease
          returnA -< (ao, Nothing)
        _        -> do
          aid <- constM myId -< ()
          returnA -< error $ "Agent " ++ show aid ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTick :: RandomGen g 
           => SugarScapeScenario
           -> DTime
           -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
handleTick params dt = do
  agentAgeing dt
  
  harvestAmount <- agentMove params 
  metabAmount   <- agentMetabolism params 
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
      return (aoDie, Nothing))
    (do 
      let cont = agentContAfterMating params 
      agentMating params agentMsf cont)

agentContAfterMating :: RandomGen g 
                     => SugarScapeScenario
                     -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterMating params = do
    aoCulture       <- agentCultureProcess params 
    (aoTrade, mhdl) <- agentTrade params cont

    let ao = aoCulture `agentOutMergeRightObs` aoTrade
    return (ao, mhdl)
  where
    cont = agentContAfterTrading params

agentContAfterTrading :: RandomGen g 
                      => SugarScapeScenario
                      -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterTrading params = do
  (aoLoan, mhdl) <- agentLoan params (agentContAfterLoan params)
  return (aoLoan, mhdl)

agentContAfterLoan :: RandomGen g 
                   => SugarScapeScenario
                   -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentContAfterLoan params = do
    (aoDisease, mhdl) <- agentDisease params cont
    return (aoDisease, mhdl)
  where
    cont = defaultCont params

defaultCont :: RandomGen g 
            => SugarScapeScenario
            -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
defaultCont params = do
  ao <- agentObservableM
  return (ao, Just $ generalEventHandler params)