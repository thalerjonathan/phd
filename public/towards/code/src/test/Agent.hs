module Agent
  ( agentTests
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Discrete
import SugarScape.Environment
import SugarScape.Model
import SugarScape.Simulation

import Runner

instance Arbitrary SugAgentState where
  -- arbitrary :: Gen SugAgentState
  arbitrary = do
    randSugarMetab     <- choose (1, 4)
    randVision         <- choose (1, 6)
    randSugarEndowment <- choose (5.0, 25.0)

    return SugAgentState {
      sugAgCoord      = (0, 0)
    , sugAgSugarMetab = randSugarMetab
    , sugAgVision     = randVision
    , sugAgSugarLevel = randSugarEndowment
    }

agentTests :: RandomGen g 
           => g
           -> TestTree 
agentTests g = testGroup "Agent Tests"
            [ QC.testProperty "Starved To Death" $ prop_agent_starved g
            , QC.testProperty "Metabolism" $ prop_agent_metabolism g]

prop_agent_starved :: RandomGen g 
                   => g
                   -> SugAgentState
                   -> Double
                   -> Bool
prop_agent_starved g0 asInit sugLvl 
    = starved == (sugLvl <= 0) &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    as0 = asInit { sugAgSugarLevel = sugLvl }
    absState0 = defaultAbsState
    env0      = emptyEnvironment

    (starved, as', absState', env', _) = runAgentMonad starvedToDeath as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'

prop_agent_metabolism :: RandomGen g 
                      => g
                      -> SugAgentState
                      -> Bool
prop_agent_metabolism g0 as0 
    = isDead ao == metabKills &&
      asUnchanged &&
      absStateUnchanged &&
      envUnchanged
  where
    absState0 = defaultAbsState
    -- TODO: we need a proper environment here, with the agent occupying it
    env0      = emptyEnvironment

    metabKills = sugAgSugarMetab as0 >= sugAgSugarLevel as0

    (ao, as', absState', env', _) = runAgentMonad agentMetabolism as0 absState0 env0 g0
    asUnchanged       = as0 == as'
    absStateUnchanged = absState0 == absState'
    envUnchanged      = env0 == env'

emptyEnvironment :: SugEnvironment
emptyEnvironment = createDiscrete2d (0, 0) moore WrapBoth []