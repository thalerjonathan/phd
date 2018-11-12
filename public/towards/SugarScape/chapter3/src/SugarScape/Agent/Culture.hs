-- {-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Culture 
  ( agentCultureProcess

  , handleCulturalProcess
  ) where

import Data.Maybe

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

agentCultureProcess :: RandomGen g
                    => SugarScapeParams               -- parameters of the current sugarscape scenario
                    -> AgentId                        -- the id of the agent 
                    -> AgentAction g (SugAgentOut g)
agentCultureProcess params _myId 
    | isNothing $ spCulturalProcess params = agentObservableM
    | otherwise = do
      -- simply broadcast to all neighbours, they compute and flip their tags themselves
      coord   <- agentProperty sugAgCoord
      ns      <- lift $ lift $ neighboursM coord False

      let neighbourIds = map (siteOccupier . snd) $ filter (siteOccupied . snd) ns

      -- no neighbours, ignore cultural process
      if null neighbourIds
        then agentObservableM
        else do
          ao         <- agentObservableM
          cultureTag <- agentProperty sugAgCultureTag
          return $ broadcastEvent neighbourIds (CulturalProcess cultureTag) ao

handleCulturalProcess :: RandomGen g
                      => AgentId
                      -> AgentId
                      -> CultureTag
                      -> AgentAction g (SugAgentOut g)
handleCulturalProcess myId _sender otherTag = do
  myTag <- agentProperty sugAgCultureTag
  
  -- NOTE: assuming length otherTag == length myTag
  idx <- lift $ lift $ lift $ getRandomR (0, length myTag - 1)

  -- when disagree, the receiving agents tag is set to the 
  -- sending agents tag => just flip it because its a Bool
  when
    (myTag !! idx /= otherTag !! idx)
    (do
      let myTag' = flipBoolAtIdx idx myTag
      updateAgentState (\s -> s { sugAgCultureTag = myTag'
                                , sugAgTribe      = tagToTribe myTag' })
      -- NOTE: need to update occupier info because tribe might have changed
      updateSiteWithOccupier myId)

  agentObservableM