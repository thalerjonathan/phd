{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Common
  ( Time
  , DTime
  , AgentId

  , ABSContext (..)

  , nextAgentId
  , getSimTime

  , mkAbsContext
  , defaultAbsContext
  ) where

import Control.Monad.State.Strict
import Control.Concurrent.STM.TVar

type Time  = Int
type DTime = Int

type AgentId = Int

data ABSContext = ABSContext
  { absNextId :: TVar AgentId
  , absTime   :: !Time
  } deriving (Show, Eq)

defaultAbsContext :: STM ABSContext
defaultAbsContext = mkAbsContext 0 

mkAbsContext :: AgentId -> STM ABSState
mkAbsContext initId = do
  aidVar <- newTVar initId + 1
  return ABSState 
    { absNextId = aidVar
    , absTime   = 0
    }