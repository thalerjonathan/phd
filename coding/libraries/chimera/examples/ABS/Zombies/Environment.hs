module Zombies.Environment (
    ZombiesEnvironmentFolding,

    zombiesEnvironmentsFold
  ) where

import           Zombies.Model

import           FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- ENVIRONMENT-COLLAPSING (parallel strategy)
------------------------------------------------------------------------------------------------------------------------
type ZombiesEnvironmentFolding = EnvironmentFolding ZombiesEnvironment

zombiesEnvironmentsFold :: ZombiesEnvironmentFolding
zombiesEnvironmentsFold es = initEnv
    where
        initEnv = head es