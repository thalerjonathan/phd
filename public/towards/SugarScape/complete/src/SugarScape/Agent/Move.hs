{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Move 
  ( agentMove
  , handleKilledInCombat
  ) where

import Data.List
import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

agentMove :: RandomGen g
          => SugarScapeScenario
          -> AgentId
          -> AgentAction g (Double, SugAgentOut g)
agentMove params myId 
  | isNothing $ spCombat params = runAgentNonCombat params myId
  | otherwise                   = agentCombat params myId

runAgentNonCombat :: RandomGen g
                  => SugarScapeScenario
                  -> AgentId
                  -> AgentAction g (Double, SugAgentOut g)
runAgentNonCombat params myId = do
  ao      <- agentObservableM
  harvest <- agentNonCombat params myId
  return (harvest, ao)

agentNonCombat :: RandomGen g
               => SugarScapeScenario
               -> AgentId
               -> AgentAction g Double
agentNonCombat params myId = do
  sitesInSight <- agentLookout
  coord        <- agentProperty sugAgCoord

  let uoc = filter (siteUnoccupied . snd) sitesInSight

  ifThenElse 
    (null uoc)
    (agentHarvestSite params myId coord)
    (do
        -- NOTE included self but this will be always kicked out because self is occupied by self, need to somehow add this
        --       what we want is that in case of same sugar on all fields (including self), the agent does not move because staying is the lowest distance (=0)
        selfCell <- envLift $ cellAtM coord
        myState  <- get

        let uoc' = (coord, selfCell) : uoc
            bf   = selectSiteMeasureFunc params myState
            bcs  = selectBestSites bf coord uoc'

        (cellCoord, _) <- randLift $ randomElemM bcs
        agentMoveTo myId cellCoord
        agentHarvestSite params myId cellCoord)

agentCombat :: RandomGen g
            => SugarScapeScenario
            -> AgentId
            -> AgentAction g (Double, SugAgentOut g)
agentCombat params myId = do
    let combatReward = fromJust $ spCombat params

    myTribe  <- agentProperty sugAgTribe
    mySugLvl <- agentProperty sugAgSugarLevel
    mySpiLvl <- agentProperty sugAgSpiceLevel
    myVis    <- agentProperty sugAgVision

    let myWealth = mySugLvl + mySpiLvl

    -- lookout in 4 directions as far as vision perimts and only consider
    -- sites occuppied by members of different tribe who are less wealthier
    sitesInSight <- agentLookout
    let sites = filter (\(_, site) -> 
                          case sugEnvSiteOccupier site of 
                            Nothing  -> False
                            Just occ -> sugEnvOccTribe occ /= myTribe &&
                                        (sugEnvOccSugarWealth occ + sugEnvOccSpiceWealth occ) < myWealth) sitesInSight

    -- throw out all sites which are vulnerable to retalation:
    nonRetaliationSites <- filterRetaliation myTribe myWealth myVis combatReward sites []

    if null nonRetaliationSites
      then runAgentNonCombat params myId -- if no sites left for combat, just do a non-combat move
      else do
        myCoord <- agentProperty sugAgCoord
      
        let bf   = combatSiteMeasure params combatReward
            bcs  = selectBestSites bf myCoord nonRetaliationSites

        (siteCoord, site) <- randLift $ randomElemM bcs
        agentMoveTo myId siteCoord
        harvestAmount <- agentHarvestSite params myId siteCoord

        let victim       = fromJust $ sugEnvSiteOccupier site
            victimWealth = sugEnvOccSugarWealth victim + sugEnvOccSpiceWealth victim
            combatWealth = min victimWealth combatReward

        let victimId = sugEnvOccId (fromJust $ sugEnvSiteOccupier site)
        ao <- fmap (sendEventTo victimId KilledInCombat) agentObservableM

        return (harvestAmount + combatWealth, ao)

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
      futureSites <- envLift $ neighboursInNeumannDistanceM sc myVis False

      let victim        = fromJust $ sugEnvSiteOccupier ss
          victimWealth  = sugEnvOccSugarWealth victim + sugEnvOccSpiceWealth victim
          combatWealth  = min victimWealth combatReward
          sugLvlSite    = sugEnvSiteSugarLevel ss
          futureWealth  = myWealth + combatWealth + sugLvlSite

          filteredSites = filter (\(_, ss') -> 
                          case sugEnvSiteOccupier ss' of 
                            Nothing  -> False
                            Just occ -> sugEnvOccTribe occ /= myTribe &&
                                        (sugEnvOccSugarWealth occ + sugEnvOccSpiceWealth occ) > futureWealth) futureSites

      -- in case sites found with agents more wealthy after this agents combat, then this site is vulnerable to retaliation, 
      if null filteredSites
        then filterRetaliation myTribe myWealth myVis combatReward sites (site : acc) -- add this site, cant be retaliated
        else filterRetaliation myTribe myWealth myVis combatReward sites acc -- ignore this site, its vulnerable to retaliation

handleKilledInCombat :: RandomGen g
                     => AgentId
                     -> AgentId
                     -> AgentAction g (SugAgentOut g)
handleKilledInCombat _myId _killerId 
  = fmap kill agentObservableM

agentLookout :: RandomGen g
             => AgentAction g [(Discrete2dCoord, SugEnvSite)]
agentLookout = do
  vis   <- agentProperty sugAgVision
  coord <- agentProperty sugAgCoord
  envLift $ neighboursInNeumannDistanceM coord vis False

agentMoveTo :: RandomGen g
             => AgentId
             -> Discrete2dCoord 
             -> AgentAction g ()
agentMoveTo myId cellCoord = do
  unoccupyPosition

  updateAgentState (\s -> s { sugAgCoord = cellCoord })

  occ  <- occupierM myId
  cell <- envLift $ cellAtM cellCoord
  let co = cell { sugEnvSiteOccupier = Just occ }
  envLift $ changeCellAtM cellCoord co 

agentHarvestSite :: RandomGen g
                 => SugarScapeScenario
                 -> AgentId
                 -> Discrete2dCoord 
                 -> AgentAction g Double
agentHarvestSite params myId siteCoord = do
  site   <- envLift $ cellAtM siteCoord
  sugLvl <- agentProperty sugAgSugarLevel
  
  let sugLvlSite = sugEnvSiteSugarLevel site

  harvestAmount <- 
    if spSpiceEnabled params 
      then do
        spiceLvl <- agentProperty sugAgSpiceLevel
        let spiceLvlSite = sugEnvSiteSpiceLevel site

        updateAgentState (\s -> s { sugAgSugarLevel = sugLvl + sugLvlSite
                                  , sugAgSpiceLevel = spiceLvl + spiceLvlSite})
        return (sugLvlSite + spiceLvlSite)

      else do
        updateAgentState (\s -> s { sugAgSugarLevel = sugLvl + sugLvlSite })
        return sugLvlSite

  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  occ <- occupierM myId

  let siteHarvested = site { sugEnvSiteSugarLevel = 0
                           , sugEnvSiteSpiceLevel = 0
                           , sugEnvSiteOccupier   = Just occ }
  envLift $ changeCellAtM siteCoord siteHarvested

  return harvestAmount

type SiteMeasureFunc = SugEnvSite -> Double

-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
sugarSiteMeasure :: SiteMeasureFunc
sugarSiteMeasure site = sug / (1 + pol)
  where
    sug = sugEnvSiteSugarLevel site
    pol = sugEnvSitePolutionLevel site

selectBestSites :: SiteMeasureFunc
                -> Discrete2dCoord
                -> [(Discrete2dCoord, SugEnvSite)]
                -> [(Discrete2dCoord, SugEnvSite)]
selectBestSites measureFunc refCoord cs = bestShortestdistanceManhattanCells
  where
    cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
    bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
    bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

    shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d refCoord (fst c1)) (distanceManhattanDisc2d refCoord (fst c2))) bestCells
    shortestdistanceManhattan = distanceManhattanDisc2d refCoord (fst $ head shortestdistanceManhattanBestCells)
    bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d refCoord) . fst) shortestdistanceManhattanBestCells
      
selectSiteMeasureFunc :: SugarScapeScenario -> SugAgentState -> SiteMeasureFunc
selectSiteMeasureFunc params as
  | spSpiceEnabled params = sugarSpiceSiteMeasure as
  | otherwise             = sugarSiteMeasure

-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
combatSiteMeasure :: SugarScapeScenario -> Double -> SiteMeasureFunc
combatSiteMeasure _params combatReward site = combatWealth + sug + spi
  where
    victim       = fromJust $ sugEnvSiteOccupier site
    victimWealth = sugEnvOccSugarWealth victim + sugEnvOccSpiceWealth victim
    combatWealth = min victimWealth combatReward

    pol          = sugEnvSitePolutionLevel site
    sug          = sugEnvSiteSugarLevel site / (1 + pol)
    spi          = sugEnvSiteSpiceLevel site / (1 + pol)

-- See page 97, The Agent Welfare Function and Appendix C (Example makes it quite clear)
-- The agent welfare function itself computes whether the agent requires more 
-- sugar or more spice, depending on the respective metabolisms. 
-- Now we apply this welfare function to compute a measure for the site which means
-- we compute the potential welfare when the agent is on that site, thus we
-- add the sites sugar / spice to the respective parts of the equation.
-- NOTE: includes polution unconditionally for better maintainability (lower number of functions and cases)
-- polution level will be 0 anyway if polution / diffusion is turned off
sugarSpiceSiteMeasure :: SugAgentState -> SiteMeasureFunc
sugarSpiceSiteMeasure as site = agentWelfareChange sug spi w1 w2 m1 m2
  where
    m1 = fromIntegral $ sugAgSugarMetab as
    m2 = fromIntegral $ sugAgSpiceMetab as
    w1 = sugAgSugarLevel as
    w2 = sugAgSpiceLevel as

    pol = sugEnvSitePolutionLevel site
    sug = sugEnvSiteSugarLevel site / (1 + pol)
    spi = sugEnvSiteSpiceLevel site / (1 + pol)