{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Credit
  ( agentCredit
  , handleCreditOffer
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils 
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
    offerLending params myId cont -- TODO: handle ao
    -- checkCredits params myId globalHdl -- TODO: handle ao

_checkCredits :: RandomGen g
             => SugarScapeScenario
             -> AgentId
             -> AgentAction g (SugAgentOut g)
_checkCredits _params _myId = undefined

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
                  => AgentId
                  -> AgentId
                  -> Double
                  -> Double
                  -> AgentAction g (SugAgentOut g)
handleCreditOffer myId lender sugar spice = do
  pb <- potentialBorrower
  case pb of
    Just reason -> do
      ao <- agentObservableM
      DBG.trace ("Agent " ++ show myId ++ ": refusing credit offer " ++ show (sugar, spice) ++ " from " ++ show lender ++ " because " ++ show reason) return (sendEventTo lender (CreditReply $ RefuseCredit reason) ao)
      
    Nothing -> do
      -- the borrower accepts the credit-offer, increase wealth of borrower
      updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sugar
                                , sugAgSpiceLevel = sugAgSpiceLevel s + spice })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteWithOccupier myId

      ao <- agentObservableM
      DBG.trace ("Agent " ++ show myId ++ ": accepts credit offer " ++ show (sugar, spice) ++ " from " ++ show lender) return (sendEventTo lender (CreditReply AcceptCredit) ao)

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

      if sugLvl > initSugLvl && spiLvl > initSpiLvl
        then return $ Just EnoughWealth -- agent has already enough wealth
        else return Nothing -- TODO check if credit-worthy if not, send NotCreditWorthy, credit worthyness: has the agent had a net income in the most recent time-step?

    else return $ Just NotFertileAge -- agent is not child-bearing age 