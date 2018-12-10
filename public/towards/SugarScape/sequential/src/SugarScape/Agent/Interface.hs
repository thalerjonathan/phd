{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module SugarScape.Agent.Interface
  ( AgentT
  , AgentMSF

  , AgentDef (..)
  , AgentOut (..)

  , agentOut
  , sendEventToAo
  , sendEventsAo
  , broadcastEventAo

  , newAgentAo
  , isDeadAo
  ) where

import Data.Tuple

import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Core.Common

type AgentT m       = StateT ABSState m
-- NOTE: an agent is a MSF not a SF! we don't need the ReaderT Double 
-- in SugarScape (we switch MSFs which would resert time anyway)
type AgentMSF m e o = MSF (AgentT m) (ABSEvent e) (Maybe (AgentOut m e o), Maybe o)

data AgentDef m e o = AgentDef
  { adId      :: AgentId
  , adSf      :: AgentMSF m e o
  , adInitObs :: o
  }

data AgentOut m e o = AgentOut 
  { aoKill       :: Bool
  , aoCreate     :: [AgentDef m e o]
  , aoEvents     :: [(AgentId, e)]   -- event receiver, (DomainEvent) event
  }

agentOut :: AgentOut m e o
agentOut = AgentOut 
  { aoKill       = False
  , aoCreate     = []
  , aoEvents     = []
  }

broadcastEventAo :: [AgentId]
                 -> e
                 -> AgentOut m e o
                 -> AgentOut m e o
broadcastEventAo rs e ao = ao'
  where
    es     = aoEvents ao
    esSend = map (swap . (,) e) rs 
    ao'    = ao { aoEvents = es ++ esSend } 

sendEventsAo :: [(AgentId, e)]
             -> AgentOut m e o
             -> AgentOut m e o
sendEventsAo es ao 
  -- important: respect ordering!
  = ao { aoEvents = aoEvents ao ++ es } 

sendEventToAo :: AgentId
              -> e
              -> AgentOut m e o
              -> AgentOut m e o
sendEventToAo receiver e ao = ao'
  where
    es  = aoEvents ao
    -- important: respect ordering!
    ao' = ao { aoEvents = es ++ [(receiver, e)] } 

newAgentAo :: AgentDef m e o
           -> AgentOut m e o 
           -> AgentOut m e o
newAgentAo adef ao 
  = ao { aoCreate = adef : aoCreate ao }

isDeadAo :: AgentOut m e o -> Bool
isDeadAo = aoKill