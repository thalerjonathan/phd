module Utils
  ( runAgentSFSteps
  , runAgentSF
  ) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict

import FRP.BearRiver

import SugarScape.AgentMonad
import SugarScape.Model
import SugarScape.Simulation (sugarScapeTimeDelta)

runAgentSFSteps :: RandomGen g
                => Int
                -> SugAgent g
                -> ABSState
                -> SugEnvironment
                -> g
                -> ([(SugAgentOut g, SugEnvironment)], SugAgent g, ABSState, g)
runAgentSFSteps steps sf0 absState0 env0 g0
    = runAgentSFStepsAux steps [] sf0 absState0 env0 g0
  where 
    runAgentSFStepsAux :: RandomGen g
                       => Int
                       -> [(SugAgentOut g, SugEnvironment)]
                       -> SugAgent g
                       -> ABSState
                       -> SugEnvironment
                       -> g
                       -> ([(SugAgentOut g, SugEnvironment)], SugAgent g, ABSState, g)
    runAgentSFStepsAux 0 acc sf absState env g = (reverse acc, sf, absState, g)
    runAgentSFStepsAux n acc sf absState env g = runAgentSFStepsAux (n - 1) acc' sf' absState' env' g'
      where
        (out, env', sf', absState', g') = runAgentSF sf absState env g
        acc' = (out, env') : acc

runAgentSF :: RandomGen g
           => SugAgent g
           -> ABSState
           -> SugEnvironment
           -> g
           -> (SugAgentOut g, SugEnvironment, SugAgent g, ABSState, g)
runAgentSF sf absState env g
    = (out, env', sf', absState', g') 
  where
    sfReader   = unMSF sf AgentIn
    sfAbsState = runReaderT sfReader sugarScapeTimeDelta
    sfEnvState = runStateT sfAbsState absState
    sfRand     = runStateT sfEnvState env
    ((((out, sf'), absState'), env'), g') = runRand sfRand g

