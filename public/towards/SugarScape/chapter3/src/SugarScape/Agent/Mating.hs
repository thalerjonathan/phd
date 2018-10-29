{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Mating 
  ( agentMating
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe
import FRP.BearRiver

import SugarScape.Agent.Common
import SugarScape.Agent.Interface
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Utils

import Debug.Trace

agentMating :: RandomGen g
            => SugarScapeParams
            -> AgentId
            -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
agentMating params aid
    | not $ spSexRuleActive params = return agentOut
    | otherwise = ifThenElseM
                    isAgentFertile
                    mateWithNeighbours
                    (return agentOut)
  where
    mateWithNeighbours :: StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
    mateWithNeighbours = do
      coord  <- agentProperty sugAgCoord
      ns     <- lift $ lift $ neighboursM coord False
      let ocs     = filter (siteOccupied . snd) ns
      let _freeNs = filter (siteUnoccupied . snd) ns
      mateWith ocs

    mateWith :: [(Discrete2dCoord, SugEnvSite)] 
             -> StateT SugAgentState (SugAgentMonadT g) (SugAgentOut g)
    mateWith [] = return agentOut
    mateWith ((c, s) : ns) = do
      agentState <- get
      let naid = sugEnvOccId $ fromJust $ sugEnvSiteOccupier s
      let cont = matingCont agentState

      return $ 
        trace ("Agent " ++ show aid ++ ": sending MatingRequest to agent " ++ show naid) 
              (sendEventToWithCont naid MatingRequest cont agentOut)

    matingCont :: SugAgentState
               -> SugAgentSF g
    matingCont s = proc evt -> do
      let ao = agentOutObservable $ sugObservableFromState s 
      returnA -< trace ("Agent " ++ show aid ++ ": holy fuck! We are in the continuation! Received " ++ show evt) ao

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
  initSugLvl <- agentProperty sugAgInitSugEndow
  return $ sugLvl >= initSugLvl