{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating

  , isAgentFertile
  , handleMatingRequest
  , handleMatingTx

  , acceptMatingRequest  -- for testing purposes
  ) where

import Data.Maybe

import Control.Monad
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
import SugarScape.Core.Utils

agentMating :: RandomGen g
            => SugarScapeAgent g              -- the top-level MSF of the agent, to be used for birthing children
            -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))  -- the action to be carried out where agentMating has left off to finalise the agent
            -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentMating amsf cont =
  ifThenElseM
    (not . spSexRuleActive <$> scenario)
    cont
    (do
      coord   <- agentProperty sugAgCoord
      ns      <- envLift $ neighboursM coord False
      fertile <- isAgentFertile

      let ocs = filter (siteOccupied . snd) ns
    
      -- note: we check at this point already if 
      --    1. there are agents 
      --    2. the agent itself is fertile
      -- This is being checked also in mateWith but when either one does not apply
      -- the switch will occur back to mainHandler from where we are coming anyway
      -- thus carrying out this extra check is a performance optimisation
      if null ocs || not fertile
        then cont
        else do
          -- shuffle ocs bcs selecting agents at random according to the book
          ocsShuff <- randLift $ randomShuffleM ocs
          mateWith amsf cont ocsShuff)

mateWith :: RandomGen g
         => SugarScapeAgent g
         -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
         -> [(Discrete2dCoord, SugEnvSite)]
         -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
mateWith _ cont [] = cont -- mating finished, continue with agent-behaviour where it left before starting mating
mateWith amsf cont ((coord, site) : ns) =
  -- check fertility again because might not be fertile because of previous matings
  ifThenElseM
    isAgentFertile
    (do
      myCoord <- agentProperty sugAgCoord
      -- always query again bcs might have changed since previous iteration
      mySites        <- envLift $ neighboursM myCoord False
      neighbourSites <- envLift $ neighboursM coord False

      -- no need to remove duplicates, bcs there cant be one with neumann neighbourhood
      let freeSites = filter (siteUnoccupied . snd) (mySites ++ neighbourSites)

      if null freeSites
        -- in case no free sites, can't give birth to new agent, try next neighbour, might have free sites
        then mateWith amsf cont ns
        else do
          -- in this case fromJust guaranteed not to fail, neighbours contain only occupied sites 
          let matingPartnerId = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site 
              evtHandler      = matingHandler amsf cont ns freeSites

          myGender <- agentProperty sugAgGender
          ao       <- agentObservableM

          return (sendEventTo matingPartnerId (MatingRequest myGender) ao, Just evtHandler))
    -- not fertile, mating finished, continue with agent-behaviour where it left before starting mating
    cont

matingHandler :: RandomGen g
              => SugarScapeAgent g
              -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
              -> [Discrete2dCell SugEnvSite]
              -> [(Discrete2dCoord, SugEnvSite)]
              -> EventHandler g
matingHandler amsf0 cont0 ns freeSites = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent sender (MatingReply accept)) -> 
            arrM (uncurry (handleMatingReply amsf0 cont0)) -< (sender, accept)
          (DomainEvent sender MatingContinue) -> do
            aid <- constM myId -< ()
            if sender /= aid 
              then returnA -< error $ "Agent " ++ show aid ++ ": received MatingContinue not from self, terminating!"
              else constM (mateWith amsf0 cont0 ns) -< ()
          _ -> do
            aid <- constM myId -< ()
            returnA -< error $ "Agent " ++ show aid ++ ": received unexpected event " ++ show evt ++ " during active Mating, terminating simulation!")
  where
    handleMatingReply :: RandomGen g
                      => SugarScapeAgent g
                      -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
                      -> AgentId
                      -> Maybe (Double, Double, Int, Int, CultureTag, ImmuneSystem)
                      -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
    handleMatingReply amsf cont _ Nothing =  -- the sender refuse the mating-request
      mateWith amsf cont ns
    handleMatingReply amsf _ sender _acc@(Just (otherSugShare, otherSpiShare, otherMetab, otherVision, otherCultureTag, otherImSysGe)) = do -- the sender accepts the mating-request
      mySugLvl  <- agentProperty sugAgSugarLevel
      mySpiLvl  <- agentProperty sugAgSpiceLevel
      myMetab   <- agentProperty sugAgSugarMetab
      myVision  <- agentProperty sugAgVision
      myCultTag <- agentProperty sugAgCultureTag
      myImSysGe <- agentProperty sugAgImSysGeno

      childMetab   <- randLift $ randomElemM [myMetab, otherMetab]
      childVision  <- randLift $ randomElemM [myVision, otherVision]
      childCultTag <- randLift $ crossOver myCultTag otherCultureTag
      childImmSys  <- randLift $ crossOver myImSysGe otherImSysGe
        
      let updateChildState s = s { sugAgSugarLevel   = (mySugLvl / 2) + otherSugShare + (mySpiLvl / 2) + otherSpiShare
                                 , sugAgSugarMetab   = childMetab
                                 , sugAgVision       = childVision
                                 , sugAgCultureTag   = childCultTag
                                 , sugAgTribe        = tagToTribe childCultTag
                                 , sugAgImmuneSystem = childImmSys
                                 , sugAgImSysGeno    = childImmSys }

      childId                 <- absStateLift nextAgentId
      (childCoord, childSite) <- randLift $ randomElemM freeSites
      -- update new-born state with its genes and initial endowment
      sc <- scenario
      (childDef, childState) <- randLift $ randomAgent sc (childId, childCoord) amsf updateChildState

      -- subtract 50% wealth, each parent provides 50% of its wealth to the child
      updateAgentState (\s -> s { sugAgSugarLevel = mySugLvl / 2
                                , sugAgSpiceLevel = mySpiLvl / 2
                                , sugAgChildren   = childId : sugAgChildren s })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteOccupied

      -- child occupies the site immediately to prevent others from occupying it
      let occ        = occupier childId childState
          childSite' = childSite { sugEnvSiteOccupier = Just occ }
      envLift $ changeCellAtM childCoord childSite' 

      -- NOTE: we need to emit an agent-out to actually give birth to the child and send a message to the 
      -- mating-partner => agent sends to itself a MatingContinue event
      ao0 <- newAgent childDef <$> agentObservableM
      -- ORDERING IS IMPORTANT: first we send the child-id to the mating-partner 
      let ao' = sendEventTo sender (MatingTx childId) ao0
      -- THEN continue with mating-requests to the remaining neighbours
      aid <- myId
      let ao'' = sendEventTo aid MatingContinue ao'
      -- NOTE: cannot continue with mateWith because sendEventTo needs to be executed before!
      return (ao'', Nothing)

crossOver :: MonadRandom m 
          => [Bool]
          -> [Bool]
          -> m [Bool]
crossOver = zipWithM selectTag
  where
    selectTag :: MonadRandom m 
              => Bool
              -> Bool
              -> m Bool
    selectTag True True   = return True
    selectTag False False = return False
    selectTag _ _         = getRandom

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
  spiLvl     <- agentProperty sugAgSpiceLevel

  initSugLvl <- agentProperty sugAgInitSugEndow
  initSpiLvl <- agentProperty sugAgInitSpiEndow
  
  return $ sugLvl >= initSugLvl && spiLvl >= initSpiLvl

handleMatingRequest :: (RandomGen g, MonadState SugAgentState m)
                    => AgentId
                    -> AgentGender
                    -> m (SugAgentOut g)
handleMatingRequest sender otherGender = do
  accept <- acceptMatingRequest otherGender
  ao     <- agentObservableM

  -- each parent provides half of its sugar-endowment for the endowment of the new-born child
  acc <- if not accept
      then return Nothing
      else do
        sugLvl  <- agentProperty sugAgSugarLevel
        spiLvl  <- agentProperty sugAgSpiceLevel
        metab   <- agentProperty sugAgSugarMetab
        vision  <- agentProperty sugAgVision
        culTag  <- agentProperty sugAgCultureTag
        imSysGe <- agentProperty sugAgImSysGeno

        return $ Just (sugLvl / 2, spiLvl / 2, metab, vision, culTag, imSysGe)

  return $ sendEventTo sender (MatingReply acc) ao

handleMatingTx :: RandomGen g
               => AgentId
               -> AgentId
               -> AgentLocalMonad g (SugAgentOut g)
handleMatingTx _sender childId = do
  sugLvl <- agentProperty sugAgSugarLevel
  spiLvl <- agentProperty sugAgSpiceLevel

  -- subtract 50% wealth, each parent provides 50% of its wealth to the child
  updateAgentState (\s -> s { sugAgSugarLevel = sugLvl / 2
                            , sugAgSpiceLevel = spiLvl / 2
                            , sugAgChildren   = childId : sugAgChildren s})
  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteOccupied

  agentObservableM