{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module SugarScape.Agent.Interface
  ( ABSEvent (..)

  , AgentT
  , AgentMSF

  , AgentDef (..)
  , AgentOut (..)

  , agentOut
  , agentOutMergeLeftObs
  , agentOutMergeRightObs
  
  , sendEventTo
  , sendEvents
  , broadcastEvent

  , isDead
  , kill
  , newAgent
  ) where

import Data.Tuple

import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Core.Common

data ABSEvent e = Tick 
                | DomainEvent (AgentId, e)  -- sender, event 
                deriving (Show, Eq) 

type AgentT m       = StateT ABSState m
-- NOTE: an agent is a MSF not a SF! we don't need the ReaderT Double 
-- in SugarScape (we switch MSFs which would resert time anyway)
type AgentMSF m e o = MSF (AgentT m) (ABSEvent e) (AgentOut m e o)

data AgentDef m e o = AgentDef
  { adId      :: AgentId
  , adSf      :: AgentMSF m e o
  , adInitObs :: o
  }

data AgentOut m e o = AgentOut 
  { aoKill       :: Bool
  , aoCreate     :: [AgentDef m e o]
  , aoObservable :: o
  , aoEvents     :: [(AgentId, e)]   -- event receiver, (DomainEvent) event
  }

agentOut :: o -> AgentOut m e o
agentOut o = AgentOut 
  { aoKill       = False
  , aoCreate     = []
  , aoObservable = o
  , aoEvents     = []
  }

agentOutMergeLeftObs :: AgentOut m e o
                     -> AgentOut m e o
                     -> AgentOut m e o
agentOutMergeLeftObs aoLeft  
    = mergeAgentOut (aoObservable aoLeft) aoLeft  

agentOutMergeRightObs :: AgentOut m e o
                      -> AgentOut m e o
                      -> AgentOut m e o
agentOutMergeRightObs aoLeft aoRight 
    = mergeAgentOut (aoObservable aoRight) aoLeft aoRight 

mergeAgentOut :: o
              -> AgentOut m e o
              -> AgentOut m e o
              -> AgentOut m e o
mergeAgentOut o aoLeft aoRight = AgentOut 
  { aoKill       = aoKill aoLeft || aoKill aoRight
  , aoCreate     = aoCreate aoLeft ++ aoCreate aoRight
  , aoObservable = o
  , aoEvents     = aoEvents aoLeft ++ aoEvents aoRight
  }

broadcastEvent :: [AgentId]
               -> e
               -> AgentOut m e o
               -> AgentOut m e o
broadcastEvent rs e ao = ao'
  where
    es     = aoEvents ao
    esSend = map (swap . (,) e) rs 
    ao'    = ao { aoEvents = es ++ esSend } 

sendEvents :: [(AgentId, e)]
           -> AgentOut m e o
           -> AgentOut m e o
sendEvents es ao 
  -- important: respect ordering!
  = ao { aoEvents = aoEvents ao ++ es } 

sendEventTo :: AgentId
            -> e
            -> AgentOut m e o
            -> AgentOut m e o
sendEventTo receiver e ao = ao'
  where
    es  = aoEvents ao
    -- important: respect ordering!
    ao' = ao { aoEvents = es ++ [(receiver, e)] } 

isDead :: AgentOut m e o -> Bool
isDead = aoKill

kill :: AgentOut m e o -> AgentOut m e o
kill ao = ao { aoKill = True }

newAgent :: AgentDef m e o
         -> AgentOut m e o 
         -> AgentOut m e o
newAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }