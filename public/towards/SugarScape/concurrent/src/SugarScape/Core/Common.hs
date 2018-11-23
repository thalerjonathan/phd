-- NOTE: if this is NOT strict, then memory builds up like HELL
{-# LANGUAGE Strict           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Core.Common
  ( Time
  , DTime
  , AgentId
  , ReplyChannel
  
  , ABSEvent (..)

  , ABSCtx (..)
  
  , mkAbsCtx
  , defaultAbsCtx
  ) where

import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import qualified Data.IntMap.Strict as Map -- better performance than normal Map according to hackage page

type Time  = Int
type DTime = Int

type AgentId = Int

type ReplyChannel e = TMVar (AgentId, e) -- sender, event

data ABSEvent e = TickStart DTime
                | TickEnd 
                | DomainEvent AgentId e  -- sender, event
                | DomainEventWithReply AgentId e (ReplyChannel e) -- NOTE: this is used to establish a multi-step Reply interaction using ReplyChannel
                | Reply AgentId e (ReplyChannel e)                -- NOTE: could have used DomainEventWithReply but Reply makes it clear that we are communicating already using a ReplyChannel

data ABSCtx e = ABSCtx
  { absCtxIdVar     :: TVar AgentId  -- holds the NEXT agent-id 
  , absCtxMsgQueues :: TVar (Map.IntMap (TQueue (ABSEvent e)))
  , absCtxTime      :: !Time
  }

instance Show e => Show (ABSEvent e) where
  show (TickStart dt) = "TickStart " ++ show dt 
  show TickEnd = "TickEnd"
  show (DomainEvent aid e) = "DomainEvent " ++ show aid ++ " (" ++ show e ++ ")"
  show (DomainEventWithReply aid e _) = "DomainEventWithReply " ++ show aid ++ " (" ++ show e ++ ")"
  show (Reply aid e _) = "Reply " ++ show aid ++ " (" ++ show e ++ ")"

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