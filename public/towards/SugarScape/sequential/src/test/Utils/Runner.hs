module Utils.Runner
  ( runAgentSFTimeSteps

  , runAgentMonad
  , runAgentMonad_
  , runAgentMonadDefaultConst

  , runSugEnvSteps

  , emptyEnvironment
  ) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State.Strict

import SugarScape.Agent.Common
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Environment
import SugarScape.Core.Model
import SugarScape.Core.Scenario
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
               => AgentLocalMonad g ret
               -> SugarScapeScenario
               -> AgentId
               -> SugAgentState
               -> ABSState
               -> SugEnvironment
               -> g
               -> ret
runAgentMonad_ f sc aid as0 absState0 env0 g0 = ret
  where
    (ret, _, _, _, _) = runAgentMonad f sc aid as0 absState0 env0 g0

-- runs a given agent computation with default values for
-- 1. scenario
-- 2. agent id
-- 3. environment
-- 4. abs-state
-- it returns the value of the computation and checks
-- whether either one of the default values have changed.
-- If any of the default values have changed it will throw an error
runAgentMonadDefaultConst :: RandomGen g
                          => AgentLocalMonad g ret
                          -> SugAgentState
                          -> g
                          -> (ret, SugAgentState)
runAgentMonadDefaultConst acomp as0 g0 
    | absState' /= absState0 = error "ABSState has changed during Agent-Computation!"
    | env' /= env0           = error "Environment has changed during Agent-Computation!"
    | otherwise              = (ret, as')
  where
    scen      = mkSugarScapeScenario
    aid       = 0
    env0      = emptyEnvironment
    absState0 = defaultAbsState

    (ret, as', absState', env', _) = runAgentMonad acomp scen aid as0 absState0 env0 g0
    
runAgentMonad :: RandomGen g
              => AgentLocalMonad g ret
              -> SugarScapeScenario
              -> AgentId
              -> SugAgentState
              -> ABSState
              -> SugEnvironment
              -> g
              -> (ret, SugAgentState, ABSState, SugEnvironment, g)
runAgentMonad f sc aid as0 absState0 env0 g0 
    = (ret, as', absState', env', g')
  where
    fAgState  = runReaderT f (sc, aid)
    fAbsState = runStateT fAgState as0
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

emptyEnvironment :: SugEnvironment
emptyEnvironment = createDiscrete2d (0, 0) moore WrapBoth []