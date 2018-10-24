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

type AgentT m    = StateT ABSState m
type Agent m o e = SF (AgentT m) e (AgentOut m o e, e)

data AgentDef m o e = AgentDef
  { adId       :: !AgentId
  , adBeh      :: Agent m o e
  }

data AgentOut m o e = AgentOut 
  { aoKill       :: !(Event ())
  , aoCreate     :: ![AgentDef m o e]
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

agentOut :: AgentOut m o e
agentOut = agentOutAux Nothing

agentOutObservable :: o -> AgentOut m o e
agentOutObservable o = agentOutAux $ Just o

agentOutAux :: Maybe o -> AgentOut m o e
agentOutAux mo = AgentOut 
  { aoKill       = NoEvent
  , aoCreate     = []
  , aoObservable = mo
  }

isDead :: AgentOut m o e -> Bool
isDead ao = isEvent $ aoKill ao

kill :: AgentOut m o e -> AgentOut m o e
kill ao = ao { aoKill = Event () }

newAgent :: AgentDef m o e
         -> AgentOut m o e
         -> AgentOut m o e
newAgent adef ao 
  = ao { aoCreate = adef : aoCreate ao }

isObservable :: AgentOut m o e -> Bool
isObservable ao = isJust $ aoObservable ao

(<°>) :: AgentOut m o e 
      -> AgentOut m o e 
      -> AgentOut m o e
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