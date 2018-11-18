{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Common
  ( Time
  , DTime
  , AgentId

  , ABSCtx (..)
  
  , mkAbsCtx
  , defaultAbsCtx
  ) where

import Control.Concurrent.STM.TVar
import Control.Monad.STM

type Time  = Int
type DTime = Int

type AgentId = Int

data ABSCtx = ABSCtx
  { absCtxIdVar :: TVar AgentId  -- holds the NEXT agent-id 
  , absCtxTime  :: !Time
  }

defaultAbsCtx :: STM ABSCtx
defaultAbsCtx = mkAbsCtx 0 

mkAbsCtx :: AgentId -> STM ABSCtx
mkAbsCtx initId = do
  aidVar <- newTVar (initId + 1)

  return ABSCtx 
    { absCtxIdVar = aidVar
    , absCtxTime  = 0
    }