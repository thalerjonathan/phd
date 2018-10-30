{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating
  , handleMatingRequest
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

import Debug.Trace as DBG

agentMating :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> EventHandler g
            -> AgentAction g (SugAgentOut g)
            -> StateT SugAgentState (SugAgentMonadT g) (Maybe (SugAgentOut g, Maybe (EventHandler g)))
agentMating params myId mainHandler0 act0
    | not $ spSexRuleActive params = return Nothing
    | otherwise                    = mateWithNeighbours mainHandler0 act0
  where
    mateWithNeighbours :: RandomGen g
                       => EventHandler g
                       -> AgentAction g (SugAgentOut g)
                       -> AgentAction g (Maybe (SugAgentOut g, Maybe (EventHandler g)))
    mateWithNeighbours mainHandler act = do
      coord   <- agentProperty sugAgCoord
      ns      <- lift $ lift $ neighboursM coord False
      fertile <- isAgentFertile

      let ocs = filter (siteOccupied . snd) ns
    
      -- note: we check at this point already if 
      --    1. there are agents 
      --    2. the agent is fertile
      -- This is being checked also in mateWith but when either one does not apply
      -- the switch will occur back to mainHandler from where we are coming anyway
      -- thus carrying out this extra check is a performance optimisation
      if null ocs || not fertile
        then DBG.trace ("Agent " ++ show myId ++ ": no neighbours (" ++ show (null ocs) ++ "), or i'm not fertile (" ++ show (not fertile) ++ ") => not initiating mating") 
              return Nothing
        else do
          let visNs = map (\(c, s) -> (sugEnvOccId $ fromJust $ sugEnvSiteOccupier s, c)) ocs
          ret <- DBG.trace ("Agent " ++ show myId ++ ": found " ++ show (length ocs) ++ " neighbours: " ++ show visNs ++ ", and i'm fertile => initiating mating") 
                  mateWith myId mainHandler act ocs
          return $ Just ret

mateWith :: RandomGen g
         => AgentId
         -> EventHandler g
         -> AgentAction g (SugAgentOut g)
         -> [(Discrete2dCoord, SugEnvSite)]
         -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
mateWith myId mainHandler0 cont0 [] = do
  -- mating finished, continue with agent-behaviour where it left before starting mating
  ao <- cont0 
  -- need to switch back into the main handler
  DBG.trace ("Agent " ++ show myId ++ ": mating finished, carry on with normal behaviour" ) (return (ao, Just mainHandler0))
mateWith myId mainHandler0 cont0 ((coord, site) : ns) =
    ifThenElseM
      isAgentFertile
      (do
        myCoord         <- agentProperty sugAgCoord
        -- always query again bcs might have changed since previous iteration
        mySites        <- lift $ lift $ neighboursM myCoord False
        neighbourSites <- lift $ lift $ neighboursM coord False

        let freeSites = filter (siteUnoccupied . snd) (mySites ++ neighbourSites)

        if null freeSites
          -- in case no free sites, can't give birth to new agent, try next neighbour, might have free sites
          then mateWith myId mainHandler0 cont0 ns
          else do
            -- in this case fromJust guaranteed not to fail, neighbours contain only occupied sites 
            let matingPartnerId = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site 
                replyHandler    = matingCont mainHandler0 cont0

            myGender <- agentProperty sugAgGender
            ao       <- agentOutObservableM

            return $ 
              DBG.trace ("Agent " ++ show myId ++ ": sending (MatingRequest " ++ show myGender ++ ") to agent " ++ show matingPartnerId) 
                    (sendEventTo matingPartnerId (MatingRequest myGender) ao, Just replyHandler))
      (do
        -- not fertile, mating finished, continue with agent-behaviour where it left before starting mating
        ao <- cont0 
        -- need to switch back into the main handler
        DBG.trace ("Agent " ++ show myId ++ ": not fertile, mating finished, carry on with normal behaviour" ) (return (ao, Just mainHandler0)))
  where
    matingCont :: RandomGen g
               => EventHandler g
               -> AgentAction g (SugAgentOut g)
               -> EventHandler g
    matingCont mainHandler cont' = 
        continueWithAfter
          (proc evt -> 
            case evt of
              (DomainEvent (sender, MatingReply accept)) -> 
                arrM (uncurry (handleMatingReply mainHandler cont')) -< (sender, accept)
              _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Mating, terminating simulation!")
      where
        handleMatingReply :: RandomGen g
                          => EventHandler g
                          -> AgentAction g (SugAgentOut g)
                          -> AgentId
                          -> Bool
                          -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
        handleMatingReply mainHandler' cont'' sender accept 
          -- the sender accepts the mating-request
          | accept = 
            -- 1. calculate new-born genes
            -- 2. create a new born and endow 
            -- 3. send the mating-partner a message: ack of mating
            -- 4. continue with next neighbour
            DBG.trace ("Agent " ++ show myId ++ ": incoming (MatingReply " ++ show accept ++ ") from agent " ++ show sender)
              --(mateWith myId mainHandler' cont'' ns)
              (error "finally an accepted a mating")
          -- the sender refuse the mating-request
          | otherwise = 
            DBG.trace ("Agent " ++ show myId ++ ": incoming (MatingReply " ++ show accept ++ ") from agent " ++ show sender)
            -- continue with next neighbour
              (mateWith myId mainHandler' cont'' ns)
    
acceptMatingRequest :: MonadState SugAgentState m
                    => AgentGender
                    -> m Bool
acceptMatingRequest otherGender = do
  myGender <- agentProperty sugAgGender
  fertile  <- isAgentFertile
  return $ (myGender /= otherGender) && fertile 

isAgentFertile :: MonadState SugAgentState m
               => m Bool
isAgentFertile = isAgentFertileAge `andM` isAgentFertileWealth

isAgentFertileAge :: MonadState SugAgentState m
                  => m Bool
isAgentFertileAge = do
  age        <- agentProperty sugAgAge
  (from, to) <- agentProperty sugAgFertAgeRange
  return $ age >= from && age <= to

isAgentFertileWealth :: MonadState SugAgentState m
                     => m Bool
isAgentFertileWealth = do
  sugLvl     <- agentProperty sugAgSugarLevel
  initSugLvl <- agentProperty sugAgInitSugEndow
  return $ sugLvl >= initSugLvl

handleMatingRequest :: (RandomGen g, MonadState SugAgentState m)
                    => AgentId
                    -> AgentId
                    -> AgentGender
                    -> m (SugAgentOut g)
handleMatingRequest myId sender otherGender = do
  accept <- acceptMatingRequest otherGender
  ao     <- agentOutObservableM
  DBG.trace ("Agent " ++ show myId ++ 
         ": incoming (MatingRequest " ++ show otherGender ++ 
         ") from agent " ++ show sender ++ 
         ", will reply with MatingReply " ++ show accept ++ "!") 
    (return $ sendEventTo sender (MatingReply accept) ao)