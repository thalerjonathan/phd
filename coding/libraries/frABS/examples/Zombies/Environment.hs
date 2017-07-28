module Zombies.Environment (
    ZombiesEnvironmentCollapsing,

    zombiesEnvironmentsCollapse
  ) where

import           Zombies.Model

import           FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-COLLAPSING (parallel strategy)
------------------------------------------------------------------------------------------------------------------------
type ZombiesEnvironmentCollapsing = EnvironmentCollapsing ZombiesEnvironment

zombiesEnvironmentsCollapse :: ZombiesEnvironmentCollapsing
zombiesEnvironmentsCollapse es = initEnv
    where
        initEnv = head es