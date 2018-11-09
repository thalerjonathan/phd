{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Move 
  ( agentMove
  , handleKilledInCombat
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Utils

agentMove :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> AgentAction g (Double, Maybe (SugAgentOut g))
agentMove params myId 
  | isNothing $ spCombat params = liftM (\v -> (v,Nothing)) $ agentNonCombat params myId
  | otherwise                   = agentCombat params myId

agentNonCombat :: RandomGen g
               => SugarScapeParams
               -> AgentId
               -> AgentAction g Double
agentNonCombat params myId = do
  sitesInSight <- agentLookout
  coord        <- agentProperty sugAgCoord

  let uoc = filter (siteUnoccupied . snd) sitesInSight

  ifThenElse 
    (null uoc)
    (agentHarvestSite params coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ cellAtM coord
        myState  <- get

        let uoc' = (coord, selfCell) : uoc
            bf   = bestSiteFunc params myState
            bcs  = selectBestSites bf coord uoc'

        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo myId cellCoord
        agentHarvestSite params cellCoord)

agentCombat :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> AgentAction g (Double, Maybe (SugAgentOut g))
agentCombat params myId = do
    let combatReward = fromJust $ spCombat params

    -- lookout in 4 directions as far as vision perimts
    myTribe  <- agentProperty sugAgTribe
    myWealth <- agentProperty sugAgSugarLevel
    myVis    <- agentProperty sugAgVision

    -- lookout in 4 directions as far as vision perimts and only consider
    -- sites occuppied by members of different tribe who are less wealthier
    sitesInSight <- agentLookout
    let sites = filter (\(_, site) -> 
                          case sugEnvSiteOccupier site of 
                            Nothing  -> False
                            Just occ -> sugEnvOccTribe occ /= myTribe &&
                                        sugEnvOccWealth occ < myWealth) sitesInSight

    -- throw out all sites which are vulnerable to retalation:
    nonRetaliationSites <- filterRetaliation myTribe myWealth myVis combatReward sites []

    if null nonRetaliationSites
      then liftM (\v -> (v, Nothing)) $ agentNonCombat params myId  -- if no sites left for combat, just do a non-combat move
      else do
        myCoord <- agentProperty sugAgCoord
      
        let bf   = bestCombatSite combatReward
            bcs  = selectBestSites bf myCoord nonRetaliationSites

        (siteCoord, site) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo myId siteCoord
        sugHarvested <- agentHarvestSite params siteCoord

        let victimWealth = sugEnvOccWealth (fromJust $ sugEnvSiteOccupier site)
            combatWealth = min victimWealth combatReward

        -- TODO: send KilledInCombat to the victim
        let victimId = sugEnvOccId (fromJust $ sugEnvSiteOccupier site)
        ao <- liftM (sendEventTo victimId KilledInCombat) agentOutObservableM

        return (sugHarvested + combatWealth, Just ao)

  where
    filterRetaliation :: RandomGen g
                      => AgentTribe
                      -> Double
                      -> Int
                      -> Double
                      -> [(Discrete2dCoord, SugEnvSite)]
                      -> [(Discrete2dCoord, SugEnvSite)]
                      -> AgentAction g [(Discrete2dCoord, SugEnvSite)]
    filterRetaliation _ _ _ _ [] acc = return acc
    filterRetaliation myTribe myWealth myVis combatReward (site@(sc, ss) : sites) acc = do
      futureSites <- lift $ lift $ neighboursInNeumannDistanceM sc myVis False

      let victimWealth  = sugEnvOccWealth (fromJust $ sugEnvSiteOccupier ss) 
          combatWealth  = min victimWealth combatReward
          sugLvlSite    = sugEnvSiteSugarLevel ss
          futureWealth  = myWealth + combatWealth + sugLvlSite

          filteredSites = filter (\(_, ss') -> 
                          case sugEnvSiteOccupier ss' of 
                            Nothing  -> False
                            Just occ -> sugEnvOccTribe occ /= myTribe &&
                                        sugEnvOccWealth occ > futureWealth) futureSites

      -- in case sites found with agents more wealthy after this agents combat, then this site is vulnerable to retaliation, 
      if null filteredSites
        then filterRetaliation myTribe myWealth myVis combatReward sites (site : acc) -- add this site, cant be retaliated
        else filterRetaliation myTribe myWealth myVis combatReward sites acc -- ignore this site, its vulnerable to retaliation

handleKilledInCombat :: RandomGen g
                     => AgentId
                     -> AgentId
                     -> AgentAction g (SugAgentOut g)
handleKilledInCombat _myId _killerId 
  = liftM kill agentOutObservableM

agentLookout :: RandomGen g
             => AgentAction g [(Discrete2dCoord, SugEnvSite)]
agentLookout = do
  vis   <- agentProperty sugAgVision
  coord <- agentProperty sugAgCoord
  lift $ lift $ neighboursInNeumannDistanceM coord vis False

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> AgentAction g ()
agentMoveTo aid cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  occ  <- occupierM aid
  cell <- lift $ lift $ cellAtM cellCoord
  let co = cell { sugEnvSiteOccupier = Just occ }
  lift $ lift $ changeCellAtM cellCoord co 

agentHarvestSite :: RandomGen g
                 => SugarScapeParams
                 -> Discrete2dCoord 
                 -> AgentAction g Double
agentHarvestSite params siteCoord = do
    site     <- lift $ lift $ cellAtM siteCoord
    sugLvl   <- agentProperty sugAgSugarLevel
    
    let sugLvlSite = sugEnvSiteSugarLevel site

    resColl <- if spSpiceEnabled params 
                then do
                  spiceLvl <- agentProperty sugAgSpiceLevel
                  let spiceLvlSite = sugEnvSiteSpiceLevel site

                  updateAgentState (\s -> s { sugAgSugarLevel = sugLvl + sugLvlSite
                                            , sugAgSpiceLevel = spiceLvl + spiceLvlSite})
                  return (sugLvlSite + spiceLvlSite)

                else do
                  updateAgentState (\s -> s { sugAgSugarLevel = sugLvl + sugLvlSite })
                  return sugLvlSite

    let siteHarvested = site { sugEnvSiteSugarLevel = 0
                            , sugEnvSiteSpiceLevel = 0 }
    lift $ lift $ changeCellAtM siteCoord siteHarvested

    return resColl