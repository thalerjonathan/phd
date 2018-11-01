{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating

  , handleMatingRequest
  , handleMatingTx
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Utils

--import Debug.Trace as DBG

-- TODO: do we really need a Maybe for EventHandler?
agentMating :: RandomGen g
            => SugarScapeParams               -- parameters of the current sugarscape scenario
            -> AgentId                        -- the id of the agent 
            -> SugarScapeAgent g              -- the top-level MSF of the agent, to be used for birthing children
            -> EventHandler g                 -- the top-level event-handler of the agent
            -> AgentAction g (SugAgentOut g)  -- the action to be carried out where agentMating has left off to finalise the agent
            -> AgentAction g (Maybe (SugAgentOut g, Maybe (EventHandler g)))
agentMating params myId amsf0 mainHandler0 finalizeAction0
  | not $ spSexRuleActive params = return Nothing
  | otherwise                    = do
    coord   <- agentProperty sugAgCoord
    ns      <- lift $ lift $ neighboursM coord False
    fertile <- isAgentFertile

    let ocs = filter (siteOccupied . snd) ns
  
    -- note: we check at this point already if 
    --    1. there are agents 
    --    2. the agent itself is fertile
    -- This is being checked also in mateWith but when either one does not apply
    -- the switch will occur back to mainHandler from where we are coming anyway
    -- thus carrying out this extra check is a performance optimisation
    if null ocs || not fertile
      then --DBG.trace ("Agent " ++ show myId ++ ": no neighbours (" ++ show (null ocs) ++ "), or i'm not fertile (" ++ show (not fertile) ++ ") => not initiating mating") 
           return Nothing
      else do
        -- shuffle ocs bcs selecting agents at random according to the book
        ocsShuff <- lift $ lift $ lift $ fisherYatesShuffleM ocs

        --let visNs = map (\(c, s) -> (sugEnvOccId $ fromJust $ sugEnvSiteOccupier s, c)) ocs
        ret <- --DBG.trace ("Agent " ++ show myId ++ ": found " ++ show (length ocs) ++ " neighbours: " ++ show visNs ++ ", and i'm fertile => initiating mating") 
               mateWith params myId amsf0 mainHandler0 finalizeAction0 ocsShuff
        return $ Just ret

mateWith :: RandomGen g
         => SugarScapeParams
         -> AgentId 
         -> SugarScapeAgent g
         -> EventHandler g
         -> AgentAction g (SugAgentOut g)
         -> [(Discrete2dCoord, SugEnvSite)]
         -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
mateWith _ _myId _ mainHandler finalizeAction [] = do
  -- mating finished, continue with agent-behaviour where it left before starting mating
  ao <- finalizeAction 
  -- need to switch back into the main handler
  --DBG.trace ("Agent " ++ show myId ++ ": no more neighbours, mating finished, carry on with normal behaviour" ) 
  (return (ao, Just mainHandler))
mateWith params myId amsf mainHandler finalizeAction ((coord, site) : ns) =
    -- check fertility again because might not be fertile because of previous matings
    ifThenElseM
      isAgentFertile
      (do
        myCoord <- agentProperty sugAgCoord
        -- always query again bcs might have changed since previous iteration
        mySites        <- lift $ lift $ neighboursM myCoord False
        neighbourSites <- lift $ lift $ neighboursM coord False

        -- no need to remove duplicates, bcs there cant be one with neumann neighbourhood
        let freeSites = filter (siteUnoccupied . snd) (mySites ++ neighbourSites)

        if null freeSites
          -- in case no free sites, can't give birth to new agent, try next neighbour, might have free sites
          then mateWith params myId amsf mainHandler finalizeAction ns
          else do
            -- in this case fromJust guaranteed not to fail, neighbours contain only occupied sites 
            let matingPartnerId = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site 
                evtHandler      = matingHandler params myId amsf mainHandler finalizeAction ns freeSites

            myGender <- agentProperty sugAgGender
            ao       <- agentOutObservableM

            return $ 
              --DBG.trace ("Agent " ++ show myId ++ ": sending (MatingRequest " ++ show myGender ++ ") to agent " ++ show matingPartnerId) 
                    (sendEventTo matingPartnerId (MatingRequest myGender) ao, Just evtHandler))
      (do
        -- not fertile, mating finished, continue with agent-behaviour where it left before starting mating
        ao <- finalizeAction 
        -- need to switch back into the main handler
        --DBG.trace ("Agent " ++ show myId ++ ": not fertile, mating finished, carry on with normal behaviour" ) 
        (return (ao, Just mainHandler)))

matingHandler :: RandomGen g
              => SugarScapeParams
              -> AgentId 
              -> SugarScapeAgent g
              -> EventHandler g
              -> AgentAction g (SugAgentOut g)
              -> [Discrete2dCell SugEnvSite]
              -> [(Discrete2dCoord, SugEnvSite)]
              -> EventHandler g
matingHandler params myId amsf0 mainHandler0 finalizeAction0 ns freeSites = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (sender, MatingReply accept)) -> 
            arrM (uncurry (handleMatingReply amsf0 mainHandler0 finalizeAction0)) -< (sender, accept)
          (DomainEvent (sender, MatingContinue)) -> 
            if sender /= myId 
              then returnA -< error $ "Agent " ++ show myId ++ ": received MatingContinue not from self, terminating!"
              else --DBG.trace ("Agent " ++ show myId ++ ": received MatingContinue from self, carry on with mating")
                    arrM_ (mateWith params myId amsf0 mainHandler0 finalizeAction0 ns) -< ()
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Mating, terminating simulation!")
  where
    handleMatingReply :: RandomGen g
                      => SugarScapeAgent g
                      -> EventHandler g
                      -> AgentAction g (SugAgentOut g)
                      -> AgentId
                      -> Maybe (Double, Int, Int)
                      -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleMatingReply amsf mainHandler finalizeAction _ Nothing =  -- the sender refuse the mating-request
      --DBG.trace ("Agent " ++ show myId ++ ": agent " ++ show sender ++ " refuses mating-reply, check next neighbour")
        -- continue with next neighbour
      (mateWith params myId amsf mainHandler finalizeAction ns)
    handleMatingReply amsf _ _ sender _acc@(Just (otherSugShare, otherMetab, otherVision)) = do -- the sender accepts the mating-request
      mySugLvl <- agentProperty sugAgSugarLevel
      myMetab  <- agentProperty sugAgSugarMetab
      myVision <- agentProperty sugAgVision

      childMetab  <- lift $ lift $ lift $ randomElemM [myMetab, otherMetab]
      childVision <- lift $ lift $ lift $ randomElemM [myVision, otherVision]

      let updateChildState = \s -> s { sugAgSugarLevel = (mySugLvl / 2) + otherSugShare
                                     , sugAgSugarMetab = childMetab
                                     , sugAgVision     = childVision }

      childId                 <- --DBG.trace ("Agent " ++ show myId ++ ": incoming (MatingReply " ++ show acc ++ ") from agent " ++ show sender) 
                                  (lift nextAgentId)
      (childCoord, childSite) <- lift $ lift $ lift $ randomElemM freeSites
      -- update new-born state with its genes and initial endowment
      (childDef, _childState) <- lift $ lift $ lift $ randomAgent params (childId, childCoord) amsf updateChildState

      -- subtract 50% wealth, each parent provides 50% of its wealth to the child
      updateAgentState (\s -> s { sugAgSugarLevel = mySugLvl / 2
                                , sugAgChildren   = childId : sugAgChildren s })

      -- child occupies the site immediately to prevent others from occupying it
      let childSite' = childSite { sugEnvSiteOccupier = Just (occupier childId) }
      lift $ lift $ changeCellAtM childCoord childSite' 

      -- NOTE: we need to emit an agent-out to actually give birth to the child and send a message to the 
      -- mating-partner => agent sends to itself a MatingContinue event
      ao0 <- liftM (newAgent childDef) agentOutObservableM
      -- ORDERING IS IMPORTANT: first we send the child-id to the mating-partner 
      let ao'  = --DBG.trace ("Agent " ++ show myId ++ ": sending (MatingTx " ++ show childId ++ ") to agent " ++ show sender)
                  sendEventTo sender (MatingTx childId) ao0
      -- THEN continue with mating-requests to the remaining neighbours
      let ao'' = --DBG.trace ("Agent " ++ show myId ++ ": sending MatingContinue to agent " ++ show myId ++ " (myself)")
                  sendEventTo myId MatingContinue ao'

      return (ao'', Nothing)

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
handleMatingRequest _myId sender otherGender = do
  accept <- acceptMatingRequest otherGender
  ao     <- agentOutObservableM

  sugLvl <- agentProperty sugAgSugarLevel
  metab  <- agentProperty sugAgSugarMetab
  vision <- agentProperty sugAgVision

  -- each parent provides half of its sugar-endowment for the endowment of the new-born child
  let acc = if accept
      then Just (sugLvl / 2, metab, vision)
      else Nothing

  --DBG.trace ("Agent " ++ show myId ++ 
  --       ": incoming (MatingRequest " ++ show otherGender ++ 
  --       ") from agent " ++ show sender ++ 
  --       ", will reply with MatingReply " ++ show accept ++ "!") 
  (return $ sendEventTo sender (MatingReply acc) ao)

handleMatingTx :: (RandomGen g, MonadState SugAgentState m)
               => AgentId
               -> AgentId
               -> AgentId
               -> m (SugAgentOut g)
handleMatingTx _myId _sender childId = do
  sugLvl <- agentProperty sugAgSugarLevel
  -- subtract 50% wealth, each parent provides 50% of its wealth to the child
  updateAgentState (\s -> s { sugAgSugarLevel = sugLvl / 2
                            , sugAgChildren   = childId : sugAgChildren s})

  --DBG.trace ("Agent " ++ show myId ++ 
  --       ": incoming (MatinTx " ++ show childId ++ 
  --       ") from agent " ++ show sender) 
  agentOutObservableM