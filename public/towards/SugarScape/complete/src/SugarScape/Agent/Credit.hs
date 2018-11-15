{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Credit
  ( agentCredit

  , handleCreditOffer
  , handleCreditPayback
  , handleCreditInherit
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario

import Debug.Trace as DBG

agentCredit :: RandomGen g
            => SugarScapeScenario               -- parameters of the current sugarscape scenario
            -> AgentId                        -- the id of the agent 
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentCredit params myId cont
  | isNothing $ spCreditEnabled params = cont
  | otherwise = do
    aoDebt         <- checkCredits params myId
    (aoLend, mhdl) <- offerLending params myId cont

    let ao = aoDebt `agentOutMergeRightObs` aoLend
    return (ao, mhdl)

checkCredits :: RandomGen g
             => SugarScapeScenario
             -> AgentId
             -> AgentAction g (SugAgentOut g)
checkCredits params _myId = do
    cs <- agentProperty sugAgLenders

    ret <- mapM checkCredit cs
    let (aos, mcs, debts) = unzip3 ret
        cs'               = catMaybes mcs
        debtSum           = sum debts

    -- reduce the net-income by the debts payed back from credits
    updateAgentState (\s -> s { sugAgLenders   = cs'
                              , sugAgNetIncome = sugAgNetIncome s - debtSum})

    ao0 <- agentObservableM
    return $ foldr agentOutMergeRightObs ao0 aos
  where
    checkCredit :: RandomGen g
                => Credit
                -> AgentAction g (SugAgentOut g, Maybe Credit, Double) 
    checkCredit c@(Credit dueDate lender sugarFace spiceFace) = do
      t  <- absStateLift getSimTime
      ao <- agentObservableM

      if dueDate < t
        then return (ao, Just c, 0) -- credit not yet due
        else do
          let rate   = (snd . fromJust $ spCreditEnabled params) / 100
              sugPay = sugarFace + (sugarFace * rate)  -- payback the original face-value + a given percentage (interest)
              spiPay = spiceFace + (spiceFace * rate)  -- payback the original face-value + a given percentage (interest)

          sugLvl <- agentProperty sugAgSugarLevel
          spiLvl <- agentProperty sugAgSpiceLevel
          
          if sugLvl >= sugPay && spiLvl >= spiPay
            then do -- own enough wealth to pay back the credit fully
              let ao' = sendEventTo lender (CreditPayback sugPay spiPay) ao
              return (ao', Nothing, sugPay + spiPay)
            else do -- not enough wealth, just pay back half of wealth and issue new credit for the remaining face value(s)
              let dueDate' = t + (fst . fromJust $ spCreditEnabled params)
                  sugPay'  = sugLvl / 2
                  spiPay'  = spiLvl / 2
                  c'       = Credit dueDate' lender (sugarFace - sugPay') (spiceFace - spiPay')
              
                  ao' = sendEventTo lender (CreditPayback sugPay' spiPay') ao

              return (ao', Just c', sugPay' + spiPay')

offerLending :: RandomGen g
             => SugarScapeScenario               -- parameters of the current sugarscape scenario
             -> AgentId                        -- the id of the agent 
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
offerLending params myId cont = do
  pl <- potentialLender
  if isNothing pl 
    then cont
    else do
      myCoord <- agentProperty sugAgCoord
      sites   <- envLift $ neighboursM myCoord False
      let ns = mapMaybe (sugEnvSiteOccupier . snd) sites

      if null ns
        then cont
        else do
          ns' <- randLift $ fisherYatesShuffleM ns
          lendTo params myId cont ns'

lendTo :: RandomGen g
       => SugarScapeScenario
       -> AgentId
       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
       -> [SugEnvSiteOccupier]
       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
lendTo _ myId cont [] = DBG.trace ("Agent " ++ show myId ++ ": finished offering all neighbours")  cont       -- iterated through all neighbours, finished, quit lendingTo and switch back to globalHandler
lendTo params myId cont (neighbour : ns) = do 
  pl <- potentialLender
  case pl of 
    Nothing -> DBG.trace ("Agent " ++ show myId ++ ": not potential lender anymore") cont -- not potential lender, quit
    Just (sug, spi) -> do
      let evtHandler = lendingToHandler params myId cont ns (sug, spi)
          borrowerId = sugEnvOccId neighbour

      ao <- agentObservableM
      DBG.trace ("Agent " ++ show myId ++ ": offering credit " ++ show (sug, spi) ++ " to " ++ show borrowerId ++ "...") return (sendEventTo borrowerId (CreditOffer sug spi) ao, Just evtHandler)

lendingToHandler :: RandomGen g
                 => SugarScapeScenario
                 -> AgentId
                 -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
                 -> [SugEnvSiteOccupier]
                 -> (Double, Double)
                 -> EventHandler g
lendingToHandler params myId cont0 ns (sug, spi) = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (borrowerId, CreditReply reply)) -> 
            arrM (uncurry (handleLendingReply cont0)) -< (borrowerId, reply)
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during lending, terminating simulation!")
  where
    handleLendingReply :: RandomGen g
                       => AgentAction g (SugAgentOut g, Maybe (EventHandler g))
                       -> AgentId
                       -> CreditReply
                       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleLendingReply cont borrowerId (RefuseCredit reason) =  
      -- the sender refuses the credit-offer, continue with the next neighbour
      DBG.trace ("Agent " ++ show myId ++ ": borrower " ++ show borrowerId ++ " refuses credit of " ++ show (sug, spi) ++ " because " ++ show reason) lendTo params myId cont ns
    handleLendingReply cont borrowerId AcceptCredit = do 
      -- the sender accepts the credit-offer, remove credit-wealth from lender
      updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s - sug
                                , sugAgSpiceLevel = sugAgSpiceLevel s - spi })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteWithOccupier myId

      -- continue with next neighbour
      DBG.trace ("Agent " ++ show myId ++ ": borrower " ++ show borrowerId ++ " accepts credit of " ++ show (sug, spi)) lendTo params myId cont ns

handleCreditOffer :: RandomGen g
                  => SugarScapeScenario
                  -> AgentId
                  -> AgentId
                  -> Double
                  -> Double
                  -> AgentAction g (SugAgentOut g)
handleCreditOffer params myId lender sugarFace spiceFace = do
  pb <- potentialBorrower
  case pb of
    Just reason -> do
      ao <- agentObservableM
      DBG.trace ("Agent " ++ show myId ++ ": refusing credit offer " ++ show (sugarFace, spiceFace) ++ " from " ++ show lender ++ " because " ++ show reason) return (sendEventTo lender (CreditReply $ RefuseCredit reason) ao)

    Nothing -> do
      -- the borrower accepts the credit-offer, increase wealth of borrower
      updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sugarFace
                                , sugAgSpiceLevel = sugAgSpiceLevel s + spiceFace })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteWithOccupier myId

      -- add a new credit to the borrowers obligations
      t <- absStateLift getSimTime
      let dueDate = t + (fst . fromJust $ spCreditEnabled params)
      updateAgentState (\s -> s { sugAgLenders = Credit dueDate lender sugarFace spiceFace : sugAgLenders s })

      ao <- agentObservableM
      DBG.trace ("Agent " ++ show myId ++ ": accepts credit offer " ++ show (sugarFace, spiceFace) ++ " from " ++ show lender) return (sendEventTo lender (CreditReply AcceptCredit) ao)

handleCreditPayback :: RandomGen g
                    => AgentId
                    -> AgentId
                    -> Double
                    -> Double
                    -> AgentAction g (SugAgentOut g)
handleCreditPayback _myId _borrower _sugar _spice = do
  -- TODO: we know that its fully paid back if sugar and spice equals face
  -- TODO: attention, there could be more than 1 credit to the same borrower
  ao <- agentObservableM
  return ao

handleCreditInherit :: RandomGen g
                    => AgentId
                    -> AgentId
                    -> [AgentId]
                    -> AgentAction g (SugAgentOut g)
handleCreditInherit _myId _lender _children = do
  -- TODO: handle credit-inheritance: remove the credit and split it amongst all children
  ao <- agentObservableM
  return ao

potentialLender :: MonadState SugAgentState m => m (Maybe (Double, Double))
potentialLender = do
  age                <- agentProperty sugAgAge
  (fertMin, fertMax) <- agentProperty sugAgFertAgeRange

  sugLvl <- agentProperty sugAgSugarLevel
  spiLvl <- agentProperty sugAgSpiceLevel

  if age > fertMax
    then return $ Just (sugLvl / 2, spiLvl / 2) -- agent is too old to bear children, will lend half of its current wealth
    else -- agent is within child-bearing age 
      if age >= fertMin && age <= fertMax
        then do 
          initSugLvl <- agentProperty sugAgInitSugEndow
          initSpiLvl <- agentProperty sugAgInitSpiEndow

          -- TODO: handle spice on/off
          -- check if this agent has wealth excess
          if sugLvl > initSugLvl && spiLvl > initSpiLvl
            then return $ Just (sugLvl - initSugLvl, spiLvl - initSpiLvl) -- lend excess wealth
            else return Nothing -- no wealth excess
        else return Nothing -- not within child-bearing age, too young, nothing

potentialBorrower :: MonadState SugAgentState m => m (Maybe CreditRefuse)
potentialBorrower = do
  age                <- agentProperty sugAgAge
  (fertMin, fertMax) <- agentProperty sugAgFertAgeRange

  sugLvl <- agentProperty sugAgSugarLevel
  spiLvl <- agentProperty sugAgSpiceLevel

  if age >= fertMin && age <= fertMax
    then do
      initSugLvl <- agentProperty sugAgInitSugEndow
      initSpiLvl <- agentProperty sugAgInitSpiEndow

      -- TODO: handle spice on/off
      if sugLvl > initSugLvl && spiLvl > initSpiLvl
        then return $ Just EnoughWealth -- agent has already enough wealth
        else do
          netInc <- agentProperty sugAgNetIncome

          if netInc <= 0
            then return $ Just NotCreditWorthy -- not creditworthy: has the agent had a net income in the most recent time-step?
            else return Nothing                    

    else return $ Just NotFertileAge -- agent is not child-bearing age 