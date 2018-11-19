{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating

  , isAgentFertile
  , handleMatingRequest
  , handleMatingTx
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
            => SugarScapeScenario               -- parameters of the current sugarscape scenario
            -> AgentId                        -- the id of the agent 
            -> SugarScapeAgent g              -- the top-level MSF of the agent, to be used for birthing children
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))  -- the action to be carried out where agentMating has left off to finalise the agent
            -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentMating params myId amsf cont
  | not $ spSexRuleActive params = cont
  | otherwise = do
    coord   <- agentProperty sugAgCoord
    ns      <- envRun $ neighbours coord False
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
        ocsShuff <- randLift $ fisherYatesShuffleM ocs
        mateWith params myId amsf cont ocsShuff

mateWith :: RandomGen g
         => SugarScapeScenario
         -> AgentId 
         -> SugarScapeAgent g
         -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
         -> [(Discrete2dCoord, SugEnvSite)]
         -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
mateWith _ _ _ cont [] = cont -- mating finished, continue with agent-behaviour where it left before starting mating
mateWith params myId amsf cont ((coord, site) : ns) =
  -- check fertility again because might not be fertile because of previous matings
  ifThenElseM
    isAgentFertile
    (do
      myCoord <- agentProperty sugAgCoord
      -- always query again bcs might have changed since previous iteration
      mySites        <- envRun $ neighbours myCoord False
      neighbourSites <- envRun $ neighbours coord False

      -- no need to remove duplicates, bcs there cant be one with neumann neighbourhood
      let freeSites = filter (siteUnoccupied . snd) (mySites ++ neighbourSites)

      if null freeSites
        -- in case no free sites, can't give birth to new agent, try next neighbour, might have free sites
        then mateWith params myId amsf cont ns
        else do
          -- in this case fromJust guaranteed not to fail, neighbours contain only occupied sites 
          let matingPartnerId = sugEnvOccId $ fromJust $ sugEnvSiteOccupier site 
              evtHandler      = matingHandler params myId amsf cont ns freeSites

          myGender <- agentProperty sugAgGender
          ao       <- agentObservableM

          sendEventTo myId matingPartnerId (MatingRequest myGender)

          return (ao, Just evtHandler))
    -- not fertile, mating finished, continue with agent-behaviour where it left before starting mating
    cont

matingHandler :: RandomGen g
              => SugarScapeScenario
              -> AgentId 
              -> SugarScapeAgent g
              -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
              -> [Discrete2dCell SugEnvSite]
              -> [(Discrete2dCoord, SugEnvSite)]
              -> EventHandler g
matingHandler params myId amsf0 cont0 ns freeSites = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (sender, MatingReply accept)) -> 
            arrM (uncurry (handleMatingReply amsf0 cont0)) -< (sender, accept)
          (DomainEvent (sender, MatingContinue)) -> 
            if sender /= myId 
              then returnA -< error $ "Agent " ++ show myId ++ ": received MatingContinue not from self, terminating!"
              else constM (mateWith params myId amsf0 cont0 ns) -< ()
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Mating, terminating simulation!")
  where
    handleMatingReply :: RandomGen g
                      => SugarScapeAgent g
                      -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
                      -> AgentId
                      -> Maybe (Double, Double, Int, Int, CultureTag, ImmuneSystem)
                      -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleMatingReply amsf cont _ Nothing =  -- the sender refuse the mating-request
      mateWith params myId amsf cont ns
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

      childId                 <- nextAgentId
      (childCoord, childSite) <- randLift $ randomElemM freeSites
      -- update new-born state with its genes and initial endowment
      (childDef, childState) <- randLift $ randomAgent params (childId, childCoord) amsf updateChildState

      -- subtract 50% wealth, each parent provides 50% of its wealth to the child
      updateAgentState (\s -> s { sugAgSugarLevel = mySugLvl / 2
                                , sugAgSpiceLevel = mySpiLvl / 2
                                , sugAgChildren   = childId : sugAgChildren s })
      -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
      updateSiteWithOccupier myId

      -- child occupies the site immediately to prevent others from occupying it
      let occ        = occupier childId childState
          childSite' = childSite { sugEnvSiteOccupier = Just occ }
      envRun $ changeCellAt childCoord childSite' 

      -- NOTE: we need to emit an agent-out to actually give birth to the child and send a message to the 
      -- mating-partner => agent sends to itself a MatingContinue event
      ao <- newAgent childDef <$> agentObservableM
      -- ORDERING IS IMPORTANT: first we send the child-id to the mating-partner 
      sendEventTo myId sender (MatingTx childId)
      -- THEN continue with mating-requests to the remaining neighbours
      sendEventTo myId myId MatingContinue -- TODO: no need for sending to self, will result into retry! 

      return (ao, Nothing)

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

handleMatingRequest :: RandomGen g
                    => AgentId
                    -> AgentId
                    -> AgentGender
                    -> AgentAction g (SugAgentOut g)
handleMatingRequest myId sender otherGender = do
  accept <- acceptMatingRequest otherGender

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

  sendEventTo myId sender (MatingReply acc)
  agentObservableM

handleMatingTx :: RandomGen g
               => AgentId
               -> AgentId
               -> AgentId
               -> AgentAction g (SugAgentOut g)
handleMatingTx myId _sender childId = do
  sugLvl <- agentProperty sugAgSugarLevel
  spiLvl <- agentProperty sugAgSpiceLevel

  -- subtract 50% wealth, each parent provides 50% of its wealth to the child
  updateAgentState (\s -> s { sugAgSugarLevel = sugLvl / 2
                            , sugAgSpiceLevel = spiLvl / 2
                            , sugAgChildren   = childId : sugAgChildren s})
  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteWithOccupier myId

  agentObservableM