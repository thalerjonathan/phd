{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.AgentMonad
  ( AgentId

  , ABSState (..)
  
  , Agent
  , AgentT
  
  , AgentDef (..)
  , AgentOut (..)

  , nextAgentId

  , mkAbsState
  , defaultAbsState
  
  , agentOut
  , agentOutObservable
  , isObservable
  , isDead
  , kill
  , newAgent

  , (<°>)
  ) where

import           Data.Maybe

import           Control.Monad.State.Strict
import           FRP.BearRiver

type AgentId = Int

data ABSState = ABSState
  { absNextId :: AgentId
  , absTime   :: Time
  } deriving (Show, Eq)

type AgentT m  = (StateT ABSState m)
type Agent m o = SF (AgentT m) () (AgentOut m o)

data AgentDef m o = AgentDef
  { adId       :: !AgentId
  , adBeh      :: Agent m o
  }

data AgentOut m o = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m o]
  , aoObservable :: !(Maybe o)
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

newAgent :: AgentDef m o
         -> AgentOut m o 
         -> AgentOut m o
newAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }

isObservable :: AgentOut m o -> Bool
isObservable ao = isJust $ aoObservable ao

(<°>) :: AgentOut m o 
      -> AgentOut m o 
      -> AgentOut m o
(<°>) ao1 ao2 = AgentOut 
    { aoKill       = mergeBy (\_ _ -> ()) (aoKill ao1) (aoKill ao2)
    , aoCreate     = aoCreate ao1 ++ aoCreate ao2
    , aoObservable = decideMaybe (aoObservable ao1) (aoObservable ao2)
    }
  where
    decideMaybe :: Maybe a 
                -> Maybe a 
                -> Maybe a
    decideMaybe Nothing Nothing  = Nothing
    decideMaybe (Just a) Nothing = Just a
    decideMaybe Nothing (Just a) = Just a
    decideMaybe _       _        = error "Can't decide between two Maybes"