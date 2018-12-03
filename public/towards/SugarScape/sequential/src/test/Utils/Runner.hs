module Utils.Runner
  ( runAgentSFTimeSteps

  , runAgentMonad
  , runAgentMonad_

  , runSugEnvSteps
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Core.Common
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Simulation

runAgentSFTimeSteps :: RandomGen g
                    => Int
                    -> SugAgentMSF g
                    -> ABSState
                    -> SugEnvironment
                    -> g
                    -> ([(SugAgentOut g, SugEnvironment)], SugAgentMSF g, ABSState, g)
runAgentSFTimeSteps steps = runAgentSFTimeStepsAux steps []
  where 
    runAgentSFTimeStepsAux :: RandomGen g
                           => Int
                           -> [(SugAgentOut g, SugEnvironment)]
                           -> SugAgentMSF g
                           -> ABSState
                           -> SugEnvironment
                           -> g
                           -> ([(SugAgentOut g, SugEnvironment)], SugAgentMSF g, ABSState, g)
    runAgentSFTimeStepsAux 0 acc sf absState _ g = (reverse acc, sf, absState, g)
    runAgentSFTimeStepsAux n acc sf absState env g = runAgentSFTimeStepsAux (n - 1) acc' sf' absState' env' g'
      where
        (out, sf', absState', env', g') = runAgentSF sf (Tick sugarScapeTimeDelta) absState env g
        acc' = (out, env') : acc

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

runSugEnvSteps :: Int
               -> Time
               -> SugEnvironment
               -> SugEnvBehaviour
               -> SugEnvironment
runSugEnvSteps 0 _ env _ = env
runSugEnvSteps n t env eb = runSugEnvSteps (n - 1) (t + 1) env' eb
  where
    env' = eb t env -- ignoring 