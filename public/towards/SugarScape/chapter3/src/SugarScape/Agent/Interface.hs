{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Interface
  ( AgentId
  , ABSEvent (..)

  , ABSState (..)
  
  , AgentMSF
  , AgentT
  
  , AgentDef (..)
  , AgentOut (..)

  , nextAgentId

  , mkAbsState
  , defaultAbsState
  
  , agentOut
  
  , sendEventTo
  , sendEventToWithCont

  , isDead
  , kill
  , newAgent
  ) where

import Data.Maybe
import Data.MonadicStreamFunction

import Control.Monad.State.Strict
import FRP.BearRiver

type AgentId    = Int
data ABSEvent e = TimeStep 
                | DomainEvent (AgentId, e) 
                  deriving (Show, Eq) -- sender, event 

data ABSState = ABSState
  { absNextId :: AgentId
  , absTime   :: Time
  } deriving (Show, Eq)

type AgentT m       = StateT ABSState m
-- NOTE: an agent is a MSF not a SF! we don't need the ReaderT Double in SugarScape (we switch MSFs which would resert time anyway)
type AgentMSF m e o = MSF (AgentT m) (ABSEvent e) (AgentOut m e o)

data AgentDef m e o = AgentDef
  { adId      :: !AgentId
  , adSf      :: AgentMSF m e o
  , adInitObs :: !o
  }

data AgentOut m e o = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m e o]
  , aoObservable :: !o
  , aoEvents     :: ![(AgentId, e)]                     -- 1-directional event receiver, (DomainEvent) event
  , aoEventWCont :: !(Maybe (AgentId, e, AgentMSF m e o))  -- bi-directional event, receiver, (DomainEvent) event, continuation
  }

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

agentOut :: o -> AgentOut m e o
agentOut o = AgentOut 
  { aoKill       = NoEvent
  , aoCreate     = []
  , aoObservable = o
  , aoEvents     = []
  , aoEventWCont = Nothing
  }

sendEventTo :: AgentId
            -> e
            -> AgentOut m e o
            -> AgentOut m e o
sendEventTo receiver e ao = ao'
  where
    es  = aoEvents ao
    -- important: respect ordering!
    ao' = ao { aoEvents = es ++ [(receiver, e)] } 

sendEventToWithCont :: AgentId
                    -> e
                    -> AgentMSF m e o
                    -> AgentOut m e o
                    -> AgentOut m e o
sendEventToWithCont receiver e cont ao
    | isJust $ aoEventWCont ao = error "Event with continuation already exists!"
    | otherwise                = ao'
  where
    ao' = ao { aoEventWCont = Just (receiver, e, cont) } 

isDead :: AgentOut m e o -> Bool
isDead ao = isEvent $ aoKill ao

kill :: AgentOut m e o -> AgentOut m e o
kill ao = ao { aoKill = Event () }

newAgent :: AgentDef m e o
         -> AgentOut m e o 
         -> AgentOut m e o
newAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }