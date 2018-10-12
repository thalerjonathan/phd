module Runner
  ( runAgentSFSteps
  , runAgentSF

  , runAgentMonad
  , runAgentMonad_
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

-- runs a computation of the agent monad and discharges all context except return value of computation
-- StateT SugAgentState ((StateT ABSState m) (StateT SugEnvironment (Rand g))) ret
runAgentMonad_ :: RandomGen g
               => StateT SugAgentState (SugAgentMonadT g) ret
               -> SugAgentState
               -> ABSState
               -> SugEnvironment
               -> g
               -> ret
runAgentMonad_ f as0 absState0 env0 g0 = ret
  where
    (ret, _, _, _, _) = runAgentMonad f as0 absState0 env0 g0

runAgentMonad :: RandomGen g
              => StateT SugAgentState (SugAgentMonadT g) ret
              -> SugAgentState
              -> ABSState
              -> SugEnvironment
              -> g
              -> (ret, SugAgentState, ABSState, SugEnvironment, g)
runAgentMonad f as0 absState0 env0 g0 
    = (ret, as', absState', env', g')
  where
    fAbsState = runStateT f as0
    fEnvState = runStateT fAbsState absState0
    fRand     = runStateT fEnvState env0
    ((((ret, as'), absState'), env'), g') = runRand fRand g0