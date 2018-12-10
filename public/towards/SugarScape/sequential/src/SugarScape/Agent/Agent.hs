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
agentMsf params aid s0 = feedback s0 (proc (evt, s) ->
  case evt of
    Observe -> do
      let obs = sugObservableFromState s
      returnA -< ((Nothing, Just obs), s)
    _       -> do
      let ao0 = agentOut
      -- TODO: employ a WriterT for the AgentOut, the agent never reads from it!
      -- TODO: use a Maybe AgentOut and then merge
      (s', (ao', _)) <- runStateS (runReaderS (runStateS generalEventHandler)) -< (s, ((params, aid), (ao0, evt)))
      
      -- NOTE: only return the new ao if it has changed
      -- TODO: find out if value has actually changed?
      -- let mao = if ao' == ao0 then Nothing else Just ao'
      let obs = sugObservableFromState s

      returnA -< ((Just ao', Just obs), s'))

generalEventHandler :: RandomGen g => EventHandler g
generalEventHandler =
  -- optionally switching the top event handler 
  continueWithAfter 
    (proc evt -> 
      case evt of 
        Tick dt -> do
          mhdl <- arrM handleTick -< dt
          returnA -< ((), mhdl)

        -- MATING EVENTS
        (DomainEvent sender (MatingRequest otherGender)) -> do
          arrM (uncurry handleMatingRequest) -< (sender, otherGender)
          returnA -< ((), Nothing)
        (DomainEvent sender (MatingTx childId)) -> do
          arrM (uncurry handleMatingTx) -< (sender, childId)
          returnA -< ((), Nothing)
        
        -- INHERITANCE EVENTS
        (DomainEvent _ (Inherit share)) -> do
          arrM handleInheritance -< share
          returnA -< ((), Nothing)

        -- CULTURAL PROCESS EVENTS
        (DomainEvent sender (CulturalProcess tag)) -> do
          arrM (uncurry handleCulturalProcess) -< (sender, tag)
          returnA -< ((), Nothing)

        -- COMBAT EVENTS
        (DomainEvent sender KilledInCombat) -> do
          arrM handleKilledInCombat -< sender
          returnA -< ((), Nothing)

        -- TRADING EVENTS
        (DomainEvent sender (TradingOffer traderMrsBefore traderMrsAfter)) -> do
          arrM (uncurry3 handleTradingOffer) -< (sender, traderMrsBefore, traderMrsAfter)
          returnA -< ((), Nothing)

        -- LOAN EVENTS
        (DomainEvent _ (LoanOffer loan)) -> do
          arrM handleLoanOffer -< loan
          returnA -< ((), Nothing)
        (DomainEvent sender (LoanPayback loan sugarBack spiceBack)) -> do
          arrM (uncurry4 handleLoanPayback) -< (sender, loan, sugarBack, spiceBack)
          returnA -< ((), Nothing)
        (DomainEvent sender (LoanLenderDied children)) -> do
          arrM (uncurry handleLoanLenderDied) -< (sender, children)
          returnA -< ((), Nothing)
        (DomainEvent sender (LoanInherit loan)) -> do
          arrM (uncurry handleLoanInherit) -< (sender, loan)
          returnA -< ((), Nothing)

        -- DISEASE EVENTS
        (DomainEvent _ (DiseaseTransmit disease)) -> do
          arrM handleDiseaseTransmit -< disease
          returnA -< ((), Nothing)

        _        -> do
          aid <- constM myId -< ()
          returnA -< error $ "Agent " ++ show aid ++ ": undefined event " ++ show evt ++ " in agent, terminating!")

handleTick :: RandomGen g 
           => DTime
           -> AgentLocalMonad g (Maybe (EventHandler g))
handleTick dt = do
  agentAgeing dt
  
  harvestAmount <- agentMove
  metabAmount   <- agentMetabolism
  -- initialize net-income to gathering minus metabolism, might adjusted later during Loan-handling
  updateAgentState (\s -> s { sugAgNetIncome = harvestAmount - metabAmount
                            , sugAgTrades    = []}) -- reset trades of the current Tick
  -- compute polution and diffusion
  agentPolute harvestAmount metabAmount

  -- NOTE: ordering is important to replicate the dynamics
  -- after having aged, moved and applied metabolism, the 
  -- agent could have died already, thus not able to apply other rules
  ifThenElseM
    (starvedToDeath `orM` dieOfAge)
    (do
      agentDies agentMsf
      return Nothing)
    (agentMating agentMsf agentContAfterMating)

agentContAfterMating :: RandomGen g => AgentLocalMonad g (Maybe (EventHandler g))
agentContAfterMating = do
    agentCultureProcess
    agentTrade cont
  where
    cont = agentContAfterTrading

agentContAfterTrading :: RandomGen g  => AgentLocalMonad g (Maybe (EventHandler g))
agentContAfterTrading = agentLoan agentContAfterLoan

agentContAfterLoan :: RandomGen g => AgentLocalMonad g (Maybe (EventHandler g))
agentContAfterLoan = agentDisease cont
  where
    cont = defaultCont

defaultCont :: RandomGen g => AgentLocalMonad g (Maybe (EventHandler g))
defaultCont = return $ Just generalEventHandler