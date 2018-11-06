{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Move 
  ( agentMove
  , handleKilledInCombat
  ) where

import Data.Maybe

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random
import SugarScape.Utils

import Debug.Trace

agentMove :: RandomGen g
          => SugarScapeParams
          -> AgentId
          -> AgentAction g Double
agentMove params myId 
  | isNothing $ spCombat params = agentNonCombat params myId
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
    (agentHarvestCell coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- lift $ lift $ cellAtM coord
        
        let uoc' = (coord, selfCell) : uoc
            bf   = bestSiteFunc params
            bcs  = selectBestSites bf coord uoc'

        (cellCoord, _) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo myId cellCoord
        agentHarvestCell cellCoord)

agentCombat :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> AgentAction g Double
agentCombat params myId = do
    let combatReward = fromJust $ spCombat params

    -- TODO: can we unify it e.g. no need to use agentNonCombat in case no combat sites were found

    -- lookout in 4 directions as far as vision perimts
    sitesInSight <- agentLookout
    let occSites = filter (siteOccupied . snd) sitesInSight
    -- throw out all sites occuppied by members of agents own tribe
    myTribe <- agentProperty sugAgTribe
    let otherTribeSites = filter (\(_, site) -> sugEnvOccTribe (fromJust $ sugEnvSiteOccupier site) /= myTribe) occSites
    -- throw out all sites of different tribes who are wealthier than the agent
    myWealth <- agentProperty sugAgSugarLevel
    let lessWealthySites = filter (\(_, site) -> sugEnvOccWealth (fromJust $ sugEnvSiteOccupier site) < myWealth) otherTribeSites
    -- throw out all sites which are vulnerable to retalation: 
    nonRetaliationSites <- filterRetaliation combatReward lessWealthySites []

    if null nonRetaliationSites
      then agentNonCombat params myId  -- if no sites left for combat, just do a non-combat move
      else do
        myCoord <- agentProperty sugAgCoord

        let bf   = bestCombatSite combatReward
            bcs  = selectBestSites bf myCoord nonRetaliationSites

        (siteCoord, site) <- lift $ lift $ lift $ randomElemM bcs
        agentMoveTo myId siteCoord
        sugHarvested <- agentHarvestCell siteCoord

        let victimWealth = sugEnvOccWealth (fromJust $ sugEnvSiteOccupier site)
            combatWealth = min victimWealth combatReward

        -- TODO: send KilledInCombat to the victim

        return $ trace ("Agent " ++ show myId ++ ": kills agent on site " ++ show site) (sugHarvested + combatWealth)

  where
    filterRetaliation :: RandomGen g
                      => Double
                      -> [(Discrete2dCoord, SugEnvSite)]
                      -> [(Discrete2dCoord, SugEnvSite)]
                      -> AgentAction g [(Discrete2dCoord, SugEnvSite)]
    filterRetaliation _            []                          acc = return acc
    filterRetaliation combatReward (site@(siteCoord, siteState) : sites) acc = do
      myVis       <- agentProperty sugAgVision
      myWealth    <- agentProperty sugAgSugarLevel
      myTribe     <- agentProperty sugAgTribe
      futureSites <- lift $ lift $ neighboursInNeumannDistanceM siteCoord myVis False

      let victimWealth = sugEnvOccWealth (fromJust $ sugEnvSiteOccupier siteState) 
          combatWealth = min victimWealth combatReward
          futureWealth = myWealth + combatWealth

      let occSites         = filter (siteOccupied . snd) futureSites
          otherTribeSites  = filter (\(_, siteState') -> sugEnvOccTribe (fromJust $ sugEnvSiteOccupier siteState') /= myTribe) occSites
          moreWealthySites = filter (\(_, siteState') -> sugEnvOccWealth (fromJust $ sugEnvSiteOccupier siteState') > futureWealth) otherTribeSites
      
      -- in case sites found with agents more wealthy after this agents combat, then this site is vulnerable to retaliation, 
      if null moreWealthySites
        then filterRetaliation combatReward sites (site : acc) -- add this site, cant be retaliated
        else filterRetaliation combatReward sites acc -- ignore this site, its vulnerable to retaliation

handleKilledInCombat :: RandomGen g
                     => AgentId
                     -> AgentId
                     -> AgentAction g (SugAgentOut g)
handleKilledInCombat _myId _killerId =
  trace ("Agent " ++ show _myId ++ ": killed in combat by killer " ++ show _killerId) (liftM kill agentOutObservableM)

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

agentHarvestCell :: RandomGen g
                 => Discrete2dCoord 
                 -> AgentAction g Double
agentHarvestCell cellCoord = do
  cell   <- lift $ lift $ cellAtM cellCoord
  sugLvl <- agentProperty sugAgSugarLevel

  let sugLvlSite         = sugEnvSiteSugarLevel cell
      newSugarLevelAgent = sugLvlSite + sugLvl

  updateAgentState (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

  let cellHarvested = cell { sugEnvSiteSugarLevel = 0 }
  lift $ lift $ changeCellAtM cellCoord cellHarvested

  return sugLvlSite