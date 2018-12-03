-- NOTE: if this is NOT strict, then memory builds up like HELL
{-# LANGUAGE Strict           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Common
  ( Time
  , DTime
  , AgentId
  , ABSEvent (..)
  
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

data ABSEvent e = Tick DTime
                | DomainEvent AgentId e  -- sender, event 
                deriving (Show, Eq) 

data ABSState = ABSState
  { absNextId :: AgentId -- holds the NEXT agent-id 
  , absTime   :: Time
  } deriving (Show, Eq)

getSimTime :: MonadState ABSState m => m Time
getSimTime = gets absTime

nextAgentId :: MonadState ABSState m => m AgentId
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