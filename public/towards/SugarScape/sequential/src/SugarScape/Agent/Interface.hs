{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module SugarScape.Agent.Interface
  ( AgentT
  , AgentMSF

  , AgentDef (..)
  , AgentOut (..)

  , mkAgentOut
  , sendEventToAo
  , sendEventsAo
  , broadcastEventAo

  , newAgentAo
  , killAo
  
  , isDeadAo
  ) where

import Data.Tuple

import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Core.Common

type AgentT m       = StateT ABSState m
-- NOTE: an agent is a MSF not a SF! we don't need the ReaderT Double 
-- in SugarScape (we switch MSFs which would resert time anyway)
type AgentMSF m e o = MSF (AgentT m) (ABSEvent e) (AgentOut m e o, o)

data AgentDef m e o = AgentDef
  { adId      :: AgentId
  , adSf      :: AgentMSF m e o
  , adInitObs :: o
  }

data AgentOut m e o = AgentOut 
  { aoKill   :: Bool
  , aoCreate :: [AgentDef m e o]
  , aoEvents :: [(AgentId, e)]   -- event receiver, (DomainEvent) event
  }

instance Semigroup (AgentOut m e o) where
  (<>) = mergeAgentOut
  
-- NOTE: an AgentOut is a Monoid, this will be exploited with WriterT
instance Monoid (AgentOut m e o) where
  mappend = mergeAgentOut
  mempty  = mkAgentOut

mkAgentOut :: AgentOut m e o
mkAgentOut = AgentOut 
  { aoKill       = False
  , aoCreate     = []
  , aoEvents     = []
  }

mergeAgentOut :: AgentOut m e o
              -> AgentOut m e o
              -> AgentOut m e o
mergeAgentOut ao0 ao1 = AgentOut {
    aoKill   = aoKill ao0 || aoKill ao1
  , aoCreate = aoCreate ao0 ++ aoCreate ao1 
  , aoEvents = aoEvents ao0 ++ aoEvents ao1 -- important: respect ordering
  }

-- NOTE: all these functions are intended to be used through WriterT and thus
-- construct a new AgentOut which will be merged using Monoid properties
broadcastEventAo :: [AgentId]
                 -> e
                 -> AgentOut m e o
broadcastEventAo rs e  = mkAgentOut { aoEvents = es } 
  where
    es = map (swap . (,) e) rs 

sendEventsAo :: [(AgentId, e)]
             -> AgentOut m e o
sendEventsAo es = mkAgentOut { aoEvents = es } 
  
sendEventToAo :: AgentId
              -> e
              -> AgentOut m e o
sendEventToAo receiver e = mkAgentOut { aoEvents = [(receiver, e)] } 

newAgentAo :: AgentDef m e o
           -> AgentOut m e o 
newAgentAo adef = mkAgentOut { aoCreate = [adef] }

killAo :: AgentOut m e o 
killAo = mkAgentOut { aoKill = True }

isDeadAo :: AgentOut m e o -> Bool
isDeadAo = aoKill