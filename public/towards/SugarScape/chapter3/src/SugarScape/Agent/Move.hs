{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Move 
  ( agentMove
  ) where

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
          -> AgentAction g Double
agentMove params aid = do
  cellsInSight <- agentLookout
  coord        <- agentProperty sugAgCoord

  let uoc = filter (siteUnoccupied . snd) cellsInSight

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
        agentMoveTo aid cellCoord
        agentHarvestCell cellCoord)

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

  s <- get

  cell <- lift $ lift $ cellAtM cellCoord
  let co = cell { sugEnvSiteOccupier = Just (siteOccupier aid s) }
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