{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Common 
  ( Time
  , DTime
  , AgentId

  , ABSState (..)

  , nextAgentId
  , getSimTime

  , mkAbsState
  , defaultAbsState
  ) where

import Control.Monad.State.Strict

type Time  = Int
type DTime = Int

type AgentId = Int

data ABSState = ABSState
  { absNextId :: !AgentId
  , absTime   :: !Time
  } deriving (Show, Eq)

getSimTime :: MonadState ABSState m
            => m Time
getSimTime = gets absTime

nextAgentId :: MonadState ABSState m
            => m AgentId
nextAgentId = do
  aid <- gets absNextId
  modify (\s -> s { absNextId = aid + 1 })
  return aid
  
defaultAbsState :: ABSState
defaultAbsState = mkAbsState 0 

mkAbsState :: AgentId -> ABSState
mkAbsState initId = ABSState 
  { absNextId = initId + 1
  , absTime   = 0
  }