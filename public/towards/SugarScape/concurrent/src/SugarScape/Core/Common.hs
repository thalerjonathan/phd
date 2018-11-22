-- NOTE: if this is NOT strict, then memory builds up like HELL
{-# LANGUAGE Strict           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Common
  ( Time
  , DTime
  , AgentId
  , ABSEvent (..)

  , ABSCtx (..)
  
  , mkAbsCtx
  , defaultAbsCtx
  ) where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.IntMap.Strict as Map -- better performance than normal Map according to hackage page

type Time  = Int
type DTime = Int

type AgentId = Int

data ABSEvent e = TickStart DTime
                | TickEnd 
                | DomainEvent (AgentId, e)  -- sender, event 
                deriving (Show, Eq) 

data ABSCtx e = ABSCtx
  { absCtxIdVar     :: TVar AgentId  -- holds the NEXT agent-id 
  , absCtxMsgQueues :: TVar (Map.IntMap (TQueue (ABSEvent e)))
  , absCtxTime      :: !Time
  }

defaultAbsCtx :: STM (ABSCtx e)
defaultAbsCtx = mkAbsCtx 0 

mkAbsCtx :: AgentId -> STM (ABSCtx e)
mkAbsCtx initId = do
  aidVar <- newTVar (initId + 1)
  mqsVar <- newTVar Map.empty 

  return ABSCtx 
    { absCtxIdVar     = aidVar
    , absCtxMsgQueues = mqsVar
    , absCtxTime      = 0
    }