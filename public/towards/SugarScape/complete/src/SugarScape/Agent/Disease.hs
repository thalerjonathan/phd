{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Disease
  ( agentDisease

  , isSick
  , handleDiseaseTransmit
  ) where

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

agentDisease :: RandomGen g
             => SugarScapeScenario
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
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

isSick :: MonadState SugAgentState m => m Bool
isSick = (not . null) <$> agentProperty sugAgDiseases

transmitDisease :: RandomGen g
                => AgentAction g (SugAgentOut g)
transmitDisease = do
  myCoord <- agentProperty sugAgCoord
  ns      <- filterNeighbourIds <$> envLift (neighboursM myCoord False)
  ds      <- agentProperty sugAgDiseases

  if null ds || null ns
    then agentObservableM -- no diseases or no neighbours
    else do
      rds <- randLift $ randomElemsM (length ns) ds
      ao  <- agentObservableM

      let evts = map (\(nid, d) -> (nid, DiseaseTransmit d)) (zip ns rds)

      return $ sendEvents evts ao

immuniseAgent :: MonadState SugAgentState m => m ()
immuniseAgent = do
    is <- agentProperty sugAgImmuneSystem
    ds <- agentProperty sugAgDiseases

    let (is', ds') = foldr immunise (is, []) ds

    updateAgentState (\s -> s { sugAgImmuneSystem = is'
                              , sugAgDiseases     = ds' })
  where
    immunise :: Disease 
             -> (ImmuneSystem, [Disease]) 
             -> (ImmuneSystem, [Disease])
    immunise disease (imSys, accDis) 
        | minHam == 0 = (imSys, accDis)
        | otherwise   = (imSys', disease : accDis)
      where
        dLen = length disease

        hd = hammingDistances imSys disease
        (minHam, minHamIdx) = findMinWithIdx hd

        minSubImmSys = take dLen (drop minHamIdx imSys)

        tagIdx    = findFirstDiffIdx minSubImmSys disease
        globalIdx = minHamIdx + tagIdx
        imSys'    = flipBoolAtIdx globalIdx imSys 

handleDiseaseTransmit :: RandomGen g
                      => Disease
                      -> AgentAction g (SugAgentOut g)
handleDiseaseTransmit disease = do
  is <- agentProperty sugAgImmuneSystem

  let hd = hammingDistances is disease
      md = fst $ findMinWithIdx hd

  if 0 == md
    then agentObservableM -- substring of this disease found in immune system => immune!
    else do
      updateAgentState (\s -> s { sugAgDiseases = disease : sugAgDiseases s })
      agentObservableM