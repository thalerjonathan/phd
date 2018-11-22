{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Disease
  ( agentDisease

  , handleDiseaseTransmit
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

agentDisease :: RandomGen g
             => SugarScapeScenario
             -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
             -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentDisease params cont
  | isNothing $ spDiseasesEnabled params = cont
  | otherwise = do
    -- pass a random disease to each neighbour
    aoTrans <- transmitDisease
    -- imunise agent in each step
    immuniseAgent 
    -- merge continuation out
    (aoCont, mhdl) <- cont
    return (aoTrans `agentOutMergeRightObs` aoCont, mhdl)

transmitDisease :: RandomGen g => AgentLocalMonad g (SugAgentOut g)
transmitDisease = do
  nids <- neighbourAgentIds
  ds   <- agentProperty sugAgDiseases

  if null ds || null nids
    then agentObservableM -- no diseases or no neighbours
    else do
      rds <- randLift $ randomElemsM (length nids) ds
      ao  <- agentObservableM

      let evts = zipWith (\nid d -> (nid, DiseaseTransmit d)) nids rds
      sendEvents evts
      return ao

immuniseAgent :: MonadState SugAgentState m => m ()
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
                      -> AgentLocalMonad g (SugAgentOut g)
handleDiseaseTransmit disease = do
  is <- agentProperty sugAgImmuneSystem

  let hd = hammingDistances is disease
      md = fst $ findMinWithIdx hd

  if 0 == md
    then agentObservableM -- substring of this disease found in immune system => immune, ignore disease!
    else do
      ds <- agentProperty sugAgDiseases

      if disease `elem` ds
        then agentObservableM -- already got that disease, ignore
        else do
          updateAgentState (\s -> s { sugAgDiseases = disease : ds })
          agentObservableM