{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Disease
  ( agentDisease

  , handleDiseaseTransmit
  ) where

import Data.Maybe

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

agentDisease :: RandomGen g
             => AgentLocalMonad g (Maybe (EventHandler g))
             -> AgentLocalMonad g (Maybe (EventHandler g))
agentDisease cont =
  ifThenElseM
    (isNothing . spDiseasesEnabled <$> scenario)
    cont
    (do
      -- pass a random disease to each neighbour
      transmitDisease
      -- imunise agent in each step
      immuniseAgent 
      -- merge continuation out
      cont)

transmitDisease :: RandomGen g => AgentLocalMonad g ()
transmitDisease = do
  nids <- neighbourAgentIds
  ds   <- agentProperty sugAgDiseases

  unless (null ds || null nids)
    (do
      rds <- randLift $ randomElemsM (length nids) ds
      let evts = zipWith (\nid d -> (nid, DiseaseTransmit d)) nids rds
      sendEvents evts)

immuniseAgent :: AgentLocalMonad g ()
immuniseAgent = do
    is <- agentProperty sugAgImmuneSystem
    ds <- agentProperty sugAgDiseases

    let (is', ds') = foldr (immunise is) (is, []) ds

    updateAgentState (\s -> s { sugAgImmuneSystem = is'
                              , sugAgDiseases     = ds' })
  where
    -- NOTE: we are calculating and flipping always from the
    -- same initial immunesystem, otherwise wouldn't work bcs
    -- if one disease sees change of previous one it could
    -- undo it
    immunise :: ImmuneSystem
             -> Disease 
             -> (ImmuneSystem, [Disease]) 
             -> (ImmuneSystem, [Disease])
    immunise isRef ds (is, accDis) 
        | minHam == 0 = (is, accDis)
        | otherwise   = (is', ds : accDis)
      where
        dLen = length ds

        hd = hammingDistances isRef ds
        (minHam, minHamIdx) = findMinWithIdx hd

        minSubImmSys = take dLen (drop minHamIdx isRef)

        tagIdx    = findFirstDiffIdx minSubImmSys ds
        globalIdx = minHamIdx + tagIdx
        is'       = flipBoolAtIdx globalIdx isRef 

handleDiseaseTransmit :: RandomGen g
                      => Disease
                      -> AgentLocalMonad g ()
handleDiseaseTransmit disease = do
  is <- agentProperty sugAgImmuneSystem

  let hd = hammingDistances is disease
      md = fst $ findMinWithIdx hd

  if 0 == md
    then return () -- substring of this disease found in immune system => immune, ignore disease!
    else do
      ds <- agentProperty sugAgDiseases

      if disease `elem` ds
        then return () -- already got that disease, ignore
        else updateAgentState (\s -> s { sugAgDiseases = disease : ds })