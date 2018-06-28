{-# LANGUAGE FlexibleInstances #-}

module AgentMonad
  (
    AgentId

  --, MonadAgent (..)
  , ABSState (..)
  
  , Agent
  , AgentT
  
  , AgentDef (..)
  , AgentIn (..)
  , AgentOut (..)

  , mkAbsState

  , agentOut
  , agentOutObservable
  , isObservable
  , isDead
  , kill
  , createAgent
  ) where

import Data.Maybe

import Control.Monad.State.Strict
import FRP.BearRiver

type AgentId = Int

{-
class (Monad m) => MonadAgent m where
  nextAgentId :: m AgentId
  now :: m Time

instance (MonadAgent m) => MonadAgent (StateT ABSState m) where
  -- nextAgentId :: m AgentId
  nextAgentId = do
    aid <- gets absNextId
    modify (\s -> s { absNextId = aid + 1 })
    return aid

  -- now :: m Time
  now = gets absTime
-}

data ABSState = ABSState
  { absNextId   :: AgentId
  , absTime     :: Time
  }

type AgentT m  = (StateT ABSState m)
type Agent m o = SF (AgentT m) AgentIn (AgentOut m o)

data AgentDef m o = AgentDef
  { adId       :: !AgentId
  , adBeh      :: Agent m o
  }

data AgentIn = AgentIn { }

data AgentOut m o = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m o]
  , aoObservable :: !(Maybe o)
  }

mkAbsState :: AgentId -> ABSState
mkAbsState initId = ABSState 
  { absNextId = initId + 1
  , absTime   = 0
  }

agentOut :: AgentOut m o
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m o
agentOutObservable o = agentOutAux $ Just o

agentOutAux :: Maybe o -> AgentOut m o
agentOutAux mo = AgentOut 
  { aoKill       = NoEvent
  , aoCreate     = []
  , aoObservable = mo
  }

isDead :: AgentOut m o -> Bool
isDead ao = isEvent $ aoKill ao

kill :: AgentOut m o -> AgentOut m o
kill ao = ao { aoKill = Event () }

createAgent :: AgentDef m o
            -> AgentOut m o 
            -> AgentOut m o
createAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }

isObservable :: AgentOut m o -> Bool
isObservable ao = isJust $ aoObservable ao