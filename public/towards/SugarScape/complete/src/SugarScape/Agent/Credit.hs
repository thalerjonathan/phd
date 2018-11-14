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

agentCredit :: RandomGen g
            => SugarScapeScenario               -- parameters of the current sugarscape scenario
            -> AgentId                        -- the id of the agent 
            -> EventHandler g                 -- global event handler to switch back into after trading has finished
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentCredit params myId globalHdl
   | isNothing $ spCreditEnabled params = do
      ao <- agentObservableM
      return (ao, Nothing)
  | otherwise = do
    _ <- offerLending params myId globalHdl -- TODO: handle ao
    checkCredits params myId globalHdl -- TODO: handle ao

checkCredits :: RandomGen g
             => SugarScapeScenario               -- parameters of the current sugarscape scenario
             -> AgentId                        -- the id of the agent 
             -> EventHandler g
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
checkCredits _params _myId _globalHdl = undefined

offerLending :: RandomGen g
             => SugarScapeScenario               -- parameters of the current sugarscape scenario
             -> AgentId                        -- the id of the agent 
             -> EventHandler g                 -- global event handler to switch back into after trading has finished
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
offerLending params myId globalHdl = do
  pl <- potentialLender
  if isNothing pl 
    then do
      ao <- agentObservableM
      return (ao, Just globalHdl)
    else do
      myCoord <- agentProperty sugAgCoord
      sites <- envLift $ neighboursM myCoord False
      let ns = mapMaybe (sugEnvSiteOccupier . snd) sites

      if null ns
        then do
          ao <- agentObservableM
          return (ao, Just globalHdl)
        else do
          ns' <- randLift $ fisherYatesShuffleM ns
          lendTo params myId globalHdl ns'

lendTo :: RandomGen g
       => SugarScapeScenario
       -> AgentId
       -> EventHandler g
       -> [SugEnvSiteOccupier]
       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
lendTo _ _ globalHdl [] = do       -- iterated through all neighbours, finished 
  ao <- agentObservableM
  return (ao, Just globalHdl) -- quit lendingTo and switch back to globalHandler
lendTo params myId globalHdl (neighbour : ns) = do 
  pl <- potentialLender
  case pl of 
    Nothing -> do -- not potential lender, quit
      ao <- agentObservableM
      return (ao, Just globalHdl)
    Just (sug, spi) -> do
      let evtHandler = lendingToHandler params myId globalHdl ns (sug, spi)
      ao <- agentObservableM
      return (sendEventTo (sugEnvOccId neighbour) (CreditOffer sug spi) ao, Just evtHandler)

lendingToHandler :: RandomGen g
                 => SugarScapeScenario
                 -> AgentId
                 -> EventHandler g
                 -> [SugEnvSiteOccupier]
                 -> (Double, Double)
                 -> EventHandler g
lendingToHandler params myId globalHdl0 ns (sug, spi) = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (_, CreditReply reply)) -> 
            arrM (handleLendingReply globalHdl0) -< reply
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during lending, terminating simulation!")
  where
    handleLendingReply :: RandomGen g
                       => EventHandler g
                       -> CreditReply
                       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleLendingReply globalHdl (RefuseCredit _) =  
      -- the sender refuses the credit-offer, continue with the next neighbour
      lendTo params myId globalHdl ns
    handleLendingReply globalHdl AcceptCredit = do -- the sender accepts the credit-offer
      -- but for security reasons make sure that we cap at 0 and cant go below
      updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sug
                                , sugAgSpiceLevel = sugAgSpiceLevel s + spi })

      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteWithOccupier myId

      -- continue with next neighbour
      lendTo params myId globalHdl ns

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

handleCreditOffer :: RandomGen g
                  => AgentId
                  -> AgentId
                  -> Double
                  -> Double
                  -> AgentAction g (SugAgentOut g)
handleCreditOffer _myId _sender _sugar _spice = do
  -- TODO: check if potential borrower: 
  --  1. child bearing age
  --  2. AND not enough wealth for children
  --  3. AND has income in this period: does it have more wealth than in previous period? 

  ao <- agentObservableM
  return ao