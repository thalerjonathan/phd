{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Polution 
  ( agentPolute
  ) where

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario

agentPolute :: RandomGen g
            => SugarScapeScenario
            -> Double
            -> Double
            -> AgentAction g ()
agentPolute params s m 
    = agentPoluteAux $ spPolutionFormation params
  where
    agentPoluteAux :: RandomGen g
                   => PolutionFormation 
                   -> AgentAction g ()
    agentPoluteAux NoPolution = return ()
    agentPoluteAux (Polute a b) = do
      let polution = a * s + b * m

      (coord, c) <- agentCellOnCoord
      let c' = c { sugEnvSitePolutionLevel = sugEnvSitePolutionLevel c + polution }
      envRun $ changeCellAt coord c'